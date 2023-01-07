from telegram import Update
from telegram.ext import ApplicationBuilder, CommandHandler, ContextTypes


async def hello(update: Update, context) -> None:
    await update.message.reply_text(f'Hello {update.effective_user.first_name}')


app = ApplicationBuilder().token("5520367748:AAEALOtMUL3m8UgoKkHayCk3nYjgtTHlz4s").build()

app.add_handler(CommandHandler("hello", hello))

app.run_polling()
