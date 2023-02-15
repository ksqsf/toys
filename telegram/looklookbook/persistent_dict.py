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

