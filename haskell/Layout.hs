module Layout where {

f :: Int;
f = 1;

g :: String -> IO ();
g a = let {l = length a; r = reverse a;} in
  do {
  print a;
  print l;
  print r;
  print b;
  print c;
  print e;
  return ();
} where {b = 123;
c = 234;
e = 345;};

}
