from typing import List, Tuple
from table import Table

# Convert a text to code.
def chars2code(table: Table, text: str) -> List[Tuple[str, str]]:
    ret = []
    for c in text:
        code = table.lookup_shortest(c)
        ret.append((c, code))
    return ret
