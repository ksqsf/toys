#include <vector>
#include <iostream>
#include <iomanip>
#include <algorithm>
using namespace std;

class BigInt
{
public:
    BigInt() {}
    virtual ~BigInt() {}
    BigInt(unsigned long n);
    BigInt(vector<int>& d) : digit(d) {}

    BigInt operator+(const BigInt&) const;
    BigInt& operator+=(const BigInt&);
    BigInt operator-(const BigInt&) const;
    BigInt& operator-=(const BigInt&);
    BigInt operator*(const BigInt&) const;
    BigInt& operator*=(const BigInt&);
    BigInt& operator*=(unsigned int n);
    
    BigInt& shl(unsigned n);
    BigInt shlc(unsigned n) const;

    pair<BigInt, BigInt> split_at(unsigned int n) const;

private:
    static constexpr int BASE = 10000;
    vector<int> digit;
    friend ostream& operator<<(ostream&, const BigInt&);
};

BigInt::BigInt(unsigned long n)
{
    digit.clear();
    while (n != 0) {
        digit.push_back(n % BASE);
        n /= BASE;
    }
}

pair<BigInt,BigInt> BigInt::split_at(unsigned n) const
{
    if (n < digit.size()) {
        vector<int> lo(digit.begin(), digit.begin() + n);
        vector<int> hi(digit.begin() + n, digit.end());
        return make_pair(BigInt(lo), BigInt(hi));
    } else {
        vector<int> hi;
        vector<int> lo = digit;
        return make_pair(BigInt(lo), BigInt(hi));
    }
}

BigInt& BigInt::shl(unsigned n)
{
    if (digit.size() == 0)
        return *this;
    int l = digit.size();
    digit.resize(digit.size() + n);
    for (int i = l-1; i >= 0; --i) {
        digit[i+n] = digit[i];
    }
    for (int i = 0; i < n; ++i) {
        digit[i] = 0;
    }
    return *this;
}

BigInt BigInt::shlc(unsigned n) const
{
    BigInt x = *this;
    x.shl(n);
    return x;
}

ostream& operator<<(ostream& out, const BigInt& x) {
    if (x.digit.size() == 0) { out << "0"; }
    for (int i = x.digit.size() - 1; i >= 0; --i) {
        out << setfill('0') << setw(4) << x.digit[i] << ' ';
    }
    return out;
}

// BigInt& BigInt::operator+=(const BigInt& rhs)
// {
//     int c = 0;
//     int n = max(digit.size(), rhs.digit.size());
//     for (int i = 0; i < digit.size(); ++i) {
//         digit[i] += rhs.digit[i] + c;
//         c = digit[i] / BASE;
//         digit[i] %= BASE;
//     }
//     if (c != 0) {
//         digit.push_back(c);
//     }
//     return *this;
// }

BigInt BigInt::operator+(const BigInt& rhs) const
{
    vector<int> d;
    auto& x = digit;
    auto& y = rhs.digit;
    int n = max(x.size(), y.size());
    int c = 0;
    for (int i = 0; i < n; ++i) {
        int a = i < x.size() ? x[i] : 0;
        int b = i < y.size() ? y[i] : 0;
        int z = a + b + c;
        if (z > BASE) {
            z -= BASE;
            c = 1;
        } else {
            c = 0;
        }
        d.push_back(z);
    }
    if (c != 0)
        d.push_back(c);
    return BigInt(d);
}

BigInt& BigInt::operator-=(const BigInt& rhs)
{
    int c = 0;
    for (int i = 0; i < digit.size(); ++i) {
        int x = i >= rhs.digit.size() ? 0 : rhs.digit[i];
        digit[i] = digit[i] - c - x;
        if (digit[i] < 0) {
            digit[i] += BASE;
            c = 1;
        } else {
            c = 0;
            if (i >= rhs.digit.size()) {
                break;
            }
        }
    }
    for (int i = digit.size() - 1; i >= 0; --i) {
        if (digit[i] == 0)
            digit.pop_back();
        else
            break;
    }
    return *this;
}

BigInt BigInt::operator-(const BigInt& rhs) const
{
    BigInt x = *this;
    return x -= rhs;
}

BigInt& BigInt::operator*=(unsigned int n)
{
    unsigned long c = 0;
    for (auto& a : digit) {
        a = a * n + c;
        c = a / BASE;
        a = a % BASE;
    }
    if (c != 0)
        digit.push_back(c);
    return *this;
}

BigInt& BigInt::operator*=(const BigInt& rhs)
{
    if (digit.size() == 0 || rhs.digit.size() == 0) {
        digit.clear();
    } else if (digit.size() == 1) {
        unsigned c = digit[0];
        *this = rhs;
        *this *= c;
    } else if (rhs.digit.size() == 1) {
        *this *= rhs.digit[0];
    } else {
        int N = max(digit.size(), rhs.digit.size());
        int M = N/2;
        auto [x1,x2] = split_at(M);
        auto [y1,y2] = rhs.split_at(M);
        auto A = x2*y2;
        auto B = (x1+x2)*(y1+y2);
        auto C = x1*y1;
        auto A1 = A.shlc(N), A2 = A.shlc(M);
        auto D = C.shlc(M);
        *this = A1 + B.shlc(M) + C - (A2 + D);
    }
    return *this;
}

BigInt BigInt::operator*(const BigInt& rhs)  const
{
    auto c = *this;
    return c *= rhs;
}

int main()
{
    BigInt x(12345);
    cout << x << endl; // 12345
    auto [lo, hi] = x.split_at(1);
    cout << lo << endl // 2345
         << hi << endl;// 1
    lo *= 101;
    cout << lo << endl; // 236845
    cout << lo + hi << endl; //236846
    {
        auto x = lo;
        x.shl(2);
        cout << x << endl;
    }
    cout << lo - lo << endl;
    cout << lo * lo << endl; // 56095554025
    {
        auto x = lo * lo; // lo^2
        cout << x * x << endl; // lo^4 = 3146711181371693700625
        cout << x * x * x << endl; // lo^6 = 176516507075707417589161863765625
        cout << x * x * x * x << endl; // lo^8 = 9901791258969640208466064583334307125390625
    }
    return 0;
}
