#include <bits/stdc++.h>

using namespace std;

int main(){
    int t, n;
    cin >> t;
    while(t--){
        cin >> n;
        bool ans = true;
        char a[n], b;
        for(int i=0; i<n; i++){
            cin >> a[i];
        }

        for(int i=0; i<n; i++){
            cin >> b;
            if(b == '1' and a[i] == '1'){
                ans = false;
            }
        }
        cout << (ans ? "YES" : "NO") << '\n';
    }
}
