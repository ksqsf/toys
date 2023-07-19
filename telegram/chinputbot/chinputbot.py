import logging
import os
from telegram import InlineQueryResultArticle, InputTextMessageContent, Update
from telegram.ext import ApplicationBuilder, Updater, InlineQueryHandler, CommandHandler, CallbackContext
from collections import defaultdict
import hashlib


logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)


ALPHABET = 'abcdefghijklmnopqrstuvwxyz'
FULLWIDTH = {
    ',': '，',
    '.': '。',
    '\\': '、',
    '?': '？',
    '!': '！',
    '^': '……',
    '(': '（',
    ')': '）',
}


def load_table(path: str, code_col: int, text_col: int) -> dict:
    ret = defaultdict(list)
    with open(path, 'r') as f:
        for l in f:
            l = l.strip()
            parts = l.split()
            if len(parts) < 2:
                continue
            ret[parts[code_col]].append(parts[text_col])
    return ret

def load_table_code_texts(path: str):
    ret = defaultdict(list)
    with open(path, 'r') as f:
        for l in f:
            parts = l.strip().split()
            if len(parts)<2:
                continue
            ret[parts[0]] += parts[1:]
    return ret


schemas = [
    ('mrzc', '魔然字词', load_table('mrzc.txt', 0, 1), 4),
    ('xhyx', '小鹤音形', load_table('xhyx.txt', 1, 0), 4),
    ('wb98', '五笔98', load_table('wb98.txt', 1, 0), 4),
    ('wb86', '五笔86', load_table_code_texts('wb86.txt'), 4),
]


def split_into_units(input: str, max_length: int = 4) -> list:
    """Each input unit is a string with length not greater than @max_length."""
    ret = []
    cur_len = 0
    cur_unit = ""
    for c in input:
        if c in ALPHABET:
            if cur_len < max_length:
                cur_unit += c
                cur_len += 1
            else:
                ret.append(cur_unit)
                cur_len = 1
                cur_unit = c
        else:
            if cur_len > 0:
                ret.append(cur_unit)
                cur_len = 0
            cur_unit = ""
            cur_len = 0
            ret.append(c)  # a non-alpha is a unit by itself
    if cur_len > 0:
        ret.append(cur_unit)
    return ret

def convert_with_table(table: dict, input: str, max_length: int = 4) -> str:
    units = split_into_units(input, max_length)
    commit_text = ""
    candidates = []
    quote_flag = 0
    double_quote_flag = 0
    def commit_nth(n):
        nonlocal commit_text, candidates
        if len(candidates) > n:
            commit_text += candidates[n]
        candidates = []
    def commit_str(s):
        nonlocal commit_text, candidates
        commit_text += s
        candidates = []
    for unit in units:
        if unit[0] in ALPHABET:
            commit_nth(0)
            candidates = table.get(unit, [])
            if len(candidates) == 1 and len(unit) == max_length:
                commit_nth(0)
            elif len(candidates) == 0:
                commit_str(unit)  # empty code
            else:
                pass  # deferred to the next token
        elif unit[0] == ' ':
            if len(candidates) >= 1:
                commit_nth(0)
            else:
                commit_str(' ')
        elif unit[0] == ';':
            if len(candidates) >= 2:
                commit_nth(1)
            elif len(candidates) == 1:
                commit_nth(0)
                commit_str('；')
            else:
                commit_str('；')
        elif unit[0] == "'":
            if len(candidates) >= 3:
                commit_nth(2)
            else:
                commit_nth(0)
                if quote_flag % 2 == 0:
                    commit_str('‘')
                else:
                    commit_str('’')
                quote_flag += 1
        elif unit[0] == '"':
            commit_nth(0)
            if double_quote_flag % 2 == 0:
                commit_str('“')
            else:
                commit_str('”')
            double_quote_flag += 1
        elif len(unit) == 1:
            commit_nth(0)
            ci = unit[0]
            co = FULLWIDTH.get(ci, ci)
            commit_str(co)
        else:  # don't know what happened
            commit_nth(0)
            commit_str(unit)
    if len(candidates) > 0:
        commit_nth(0)
    return commit_text


async def inlinequery(update: Update, _: CallbackContext) -> None:
    query = update.inline_query.query
    logging.info('Converting ' + query)
    results = []
    for (schema_id, name, table, max_length) in schemas:
        output = convert_with_table(table, query, max_length) or '请输入一串' + name + '编码'
        results.append(InlineQueryResultArticle(
            id=hashlib.sha256((schema_id + '_' + query).encode()).hexdigest(),
            title=name + ': ' + output,
            input_message_content=InputTextMessageContent(output)
        ))
    await update.inline_query.answer(results)


def main() -> None:
    app = ApplicationBuilder().token(os.getenv("CHINPUT_BOT_TOKEN")).build()
    app.add_handler(InlineQueryHandler(inlinequery))
    app.run_polling()


if __name__ == '__main__':
    main()
