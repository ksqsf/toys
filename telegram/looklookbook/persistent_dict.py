# persistent_dict.py

# Copyright (C) 2022-2023 ksqsf

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import pickle
from collections.abc import MutableMapping

class persistent_dict(MutableMapping):
    def __init__(self, filename: str):
        self.filename = filename
        self.cache = self._load()

    def _load(self):
        try:
            with open(self.filename, 'rb') as f:
                obj = pickle.load(f)
            if isinstance(obj, dict):
                self.cache = obj
            else:
                self.cache = obj
        except:
            self.cache = dict()

    def _save(self):
        with open(self.filename, 'wb') as f:
            pickle.dump(self.cache, f)

    def __getitem__(self, key):
        self._load()
        return self.cache[key]

    def __setitem__(self, key, value):
        assert isinstance(self.cache, dict)
        self.cache[key] = value
        self._save()

    def __delitem__(self, key):
        del self.cache[key]
        self._save()

    def __iter__(self):
        self._load()
        return iter(self.cache)

    def __len__(self):
        self._load()
        return len(self.cache)

