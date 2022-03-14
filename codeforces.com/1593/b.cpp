#include <bits/stdc++.h>

using namespace std;

const string endings[] = { "00", "25", "50", "75" };
const int INF = 1000;

int main(){
    int t, a, i, ans;
    string s;

    cin >> t;
    while(t--){
        cin >> s;
        ans = INF;
        for(auto &e: endings){
            a = 0, i = s.length() - 1;
            while(i >= 0 && s[i] != e[1]) i--, a++;
            i--;
            while(i >= 0 && s[i] != e[0]) i--, a++;

            ans = min(ans, i < 0 ? INF : a);
        }
        cout << ans << '\n';
    }
}
