#include <bits/stdc++.h>

using namespace std;

//#region template

//#region using
using str = string; // python!
using ll = long long;
using db = double;
// pairs
using pi = pair<int, int>;
// vectors
using vi = vector<int>;
using vs = vector<str>;
// vector pairs
using vpi = vector<pi>;
//#endregion using

//#region macro
#define elif else if // python again!
#define ints(...) int __VA_ARGS__; re(__VA_ARGS__)
// pairs
#define mp make_pair
#define st first
#define nd second
// vectors
#define sz(a) (int((a).size()))
#define all(a) begin(a),end(a)
#define rall(a) (a).rbegin(),(a).rend()
#define sor(a) sort(all(a))
#define rsz resize
#define ins insert
#define pb push_back
// loops
#define FOR(i, a, b) for(int (i)=(a);(i)<(b);(i)++)
#define f0r(i, a) FOR(i,0,a)
#define f1r(i, a) FOR(i,1,a)
#define rof(i, a, b) for(int(i)=(b)-1;(i)>=(a);(i)--)
#define r0f(i, a) rof(i,0,a)
#define r1f(i, a) rof(i,1,a);
#define each(a, x) for(auto&(a):(x))
//#endregion macro

//#region constant
// graph
const int dx[4] = {
        0, -1, 1, 0 }, dy[4] = {
        1, 0, 0, -1 };
const int ddx[8] = { -1, 0, 1, -1, 1, -1, 0, 1 }, ddy[8] = { 1, 1, 1, 0, 0, -1, -1, -1 };
//#endregion constant

//#region namespace
//@formatter:off
inline namespace Helpers{
    // is_iterable https://stackoverflow.com/a/53967057
    template<class T, class=void> struct is_iterable: false_type{};
    template<class T>
    struct is_iterable<T, void_t<decltype(begin(declval<T>())), decltype(end(declval<T>()))>>: true_type{};
    template<class T> constexpr bool is_iterable_v = is_iterable<T>::value;
    // is_readable
    template<class T, class=void> struct is_readable: false_type{};
    template<class T>
    struct is_readable<T, typename std::enable_if_t<is_same_v<decltype(cin >> declval<T&>()), istream&>>>: true_type{};
    template<class T> constexpr bool is_readable_v = is_readable<T>::value;
    // is_printable https://nafe.es/posts/2020-02-29-is-printable/
    template<class T, class=void> struct is_printable: false_type{};
    template<class T>
    struct is_printable<T, typename std::enable_if_t<is_same_v<decltype(cout << declval<T>()), ostream&>>>: true_type{};
    template<class T> constexpr bool is_printable_v = is_printable<T>::value;
}
inline namespace ToString{
    template<class T> constexpr bool needs_output_v = !is_printable_v<T> && is_iterable_v<T>;

    // default
    template<class T> typename enable_if<is_printable_v<T>, str>::type ts(T t){
        stringstream ss;
        ss << fixed << setprecision(15) << t;
        return ss.str();
    }
    // bit vector to string
    template<class T> str bit_vec(T t){
        str res = "{";
        f0r(i, sz(t)) res += ts(t[i]);
        return res + "}";
    }
    str ts(vector<bool> vb){ return bit_vec(move(vb)); }
    // bitset
    template<size_t SZ> str ts(bitset<SZ> b){ return bit_vec(b); }
    // pairs
    template<class T, class U> str ts(pair<T, U> p){ return "(" + ts(p.st) + ", " + ts(p.nd) + ")"; }
    // vectors, arrays
    template<class T> typename enable_if<is_iterable_v<T>, str>::type ts_sep(T t, const str &sep){
        // to string with separator
        bool first = true; str res;
        for(const auto &x:t){
            if(not first) res += sep;
            first = false; res += ts(x);
        }
        return res;
    }
    template<class T> typename enable_if<needs_output_v<T>, str>::type ts(T t){ return "{" + ts_sep(t, ", ") + "}"; }

    // nested data structures
    template<int, class T> typename enable_if<!needs_output_v<T>, vs>::type ts_lev(const T &t){ return { ts(t) }; }
    template<int lev, class T> typename enable_if<needs_output_v<T>, vs>::type ts_lev(const T &t){
        if(lev == 0 || !sz(t)) return { ts(t) };
        vs res;
        for(const auto &x:t){
            if(sz(res)) res.back() += ",";
            vs temp = ts_lev<lev - 1>(x);
            res.ins(end(res), all(temp));
        }
        f0r(i, sz(res)){
            str bf = i ? " " : "{";
            res[i] = bf + res[i];
        }
        res.back() += "}";
        return res;
    }
}
inline namespace Input{
    template<class T> constexpr bool needs_input_v = !is_readable_v<T> && is_iterable_v<T>;
    // default
    template<class T> typename enable_if<is_readable_v<T>, void>::type re(T &x){ cin >> x; }
    // complex
    template<class T> void re(complex<T> &c){ T a, b; re(a, b); c = { a, b }; }
    // others (vectors, arrays)
    template<class T> typename enable_if<needs_input_v<T>, void>::type re(T &i){ each(x, i) re(x); }
    // pairs
    template<class T, class U> void re(pair<T, U> &p){ re(p.st, p.nd); }
    // read multiple
    template<class T, class...U> void re(T &t, U &...u){ re(t); re(u...); }
}
inline namespace Output{
    // print with separator
    template<class T> void pr_sep(ostream &os, const str &, const T &t){ os << ts(t); }
    template<class T, class...U> void pr_sep(ostream &os, str sep, const T &t, const U &...u){
        pr_sep(os, sep, t);
        os << sep;
        pr_sep(os, sep, u...);
    }
    // print without space
    template<class ...T> void pr(const T &...t){ pr_sep(cout, "", t...); }
    // print with space, end with \n
    void ps(){ cout << endl; }
    template<class ...T> void ps(const T &...t){ pr_sep(cout, " ", t...); ps(); }
}
//#endregion namespace

//@formatter:on
void io(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    #ifdef LOCAL
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);
    #endif
}
//#endregion template

int main(){
    io();

    ints(n);
    int a=0,b=0,c=0,r;
    f0r(i,n){re(r);a+=r;}
    f0r(i,n){re(r);b+=r;}
    f0r(i,n){re(r);c+=r;}
    ps(a*b*c);
}
