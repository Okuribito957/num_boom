// exact solver for black-peg mastermind, P positions, C colors, with memo
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#define P 4
#define MAXN 10000
typedef unsigned short code;
int C, NC;
static uint64_t M[MAXN]; static unsigned char DG[MAXN][P]; static uint64_t Z[MAXN];
static int G[64];
static inline int fb(int a,int b){return __builtin_popcountll(M[a]&M[b]);}
long nodes=0;
int hist[64], nh=0;
// memo
#define HB 26
typedef struct {uint64_t k; signed char yes, no;} ent; // yes: min d known solvable; no: max d known unsolvable
ent *H;
ent dummy; long hfull=0;
ent* look(uint64_t k){ uint64_t i=k&((1<<HB)-1); for(int pr=0;;pr++){ if(pr>200){hfull++;dummy.k=k;dummy.yes=100;dummy.no=-1;return &dummy;} if(H[i].k==k) return &H[i]; if(H[i].k==0){H[i].k=k;H[i].yes=100;H[i].no=-1;return &H[i];} i=(i+1)&((1<<HB)-1);} }
typedef struct {int g; long score;} cand;
static int cmpc(const void*a,const void*b){long x=((cand*)a)->score,y=((cand*)b)->score;return x<y?-1:x>y;}
int solve(code*S,int n,int d);
int solve(code*S,int n,int d){
  nodes++;
  if(n==1) return d>=1;
  if(d<=1) return 0;
  if(n>G[d]) return 0;
  uint64_t key=0x9e3779b97f4a7c15ULL*n; for(int i=0;i<n;i++) key^=Z[S[i]]; if(!key)key=1;
  ent*e=look(key);
  if(!getenv("NOMEMO")){ if(d>=e->yes) return 1; if(d<=e->no) return 0; }
  int present[P][16]; memset(present,0,sizeof present);
  for(int i=0;i<n;i++) for(int p=0;p<P;p++) present[p][DG[S[i]][p]]=1;
  int reps[P][17], nr[P];
  for(int p=0;p<P;p++){
    nr[p]=0; int keys[16]; int absentdone=0;
    for(int v=0;v<C;v++){
      if(!present[p][v]){ keys[v]=-1; if(absentdone) continue; absentdone=1; reps[p][nr[p]++]=v; continue; }
      int k=0; for(int h=0;h<nh;h++) if(DG[hist[h]][p]==v) k|=1<<h;
      int dup=0; for(int j=0;j<v;j++) if(keys[j]==k){dup=1;break;}
      keys[v]=k; if(!dup) reps[p][nr[p]++]=v;
    }
  }
  int ncand=1; for(int p=0;p<P;p++) ncand*=nr[p];
  cand *CA=malloc(sizeof(cand)*ncand); int m=0;
  int idx[P]={0};
  for(int t=0;t<ncand;t++){
    int g=0; {int tt=t; for(int p=0;p<P;p++){ g=g*C+reps[p][tt%nr[p]]; tt/=nr[p]; }}
    int cnt[P+1]={0}; for(int i=0;i<n;i++) cnt[fb(g,S[i])]++;
    int ok=1,mx=0; long sq=0;
    for(int r=0;r<P;r++){ if(cnt[r]>G[d-1]){ok=0;break;} if(cnt[r]>mx)mx=cnt[r]; sq+=(long)cnt[r]*cnt[r]; }
    if(!ok) continue;
    if(cnt[P]==0 && mx==n) continue;
    if(d==2 && mx>1) continue;
    CA[m].g=g; CA[m].score=(sq<<2)-cnt[P]*3; m++;
  }
  qsort(CA,m,sizeof(cand),cmpc);
  code *buf=malloc(sizeof(code)*n);
  int res=0;
  for(int ci=0;ci<m && !res;ci++){
    int g=CA[ci].g;
    int cnt[P+1]={0},off[P+2],pos[P+1];
    for(int i=0;i<n;i++)cnt[fb(g,S[i])]++;
    off[0]=0; for(int r=0;r<=P;r++)off[r+1]=off[r]+cnt[r];
    memcpy(pos,off,sizeof pos);
    for(int i=0;i<n;i++){int r=fb(g,S[i]); buf[pos[r]++]=S[i];}
    int order[P]; for(int r=0;r<P;r++)order[r]=r;
    for(int x=0;x<P;x++)for(int y=x+1;y<P;y++) if(cnt[order[y]]>cnt[order[x]]){int t=order[x];order[x]=order[y];order[y]=t;}
    hist[nh++]=g; int ok=1;
    for(int k=0;k<P&&ok;k++){int r=order[k]; if(cnt[r]==0)continue; if(!solve(buf+off[r],cnt[r],d-1)) ok=0;}
    nh--; res=ok;
  }
  free(buf); free(CA);
  e=look(key);
  if(res){ if(d<e->yes) e->yes=d; } else { if(d>e->no) e->no=d; }
  return res;
}
int main(int argc,char**argv){
  C=atoi(argv[1]); int which=atoi(argv[2]); int dmax=atoi(argv[3]);
  NC=1; for(int p=0;p<P;p++) NC*=C;
  H=calloc(1<<HB,sizeof(ent));
  uint64_t s=88172645463325252ULL;
  for(int i=0;i<NC;i++){int x=i;for(int p=P-1;p>=0;p--){DG[i][p]=x%C;x/=C;} M[i]=0; for(int p=0;p<P;p++) M[i]|=1ULL<<(p*C+DG[i][p]);
    s^=s<<13;s^=s>>7;s^=s<<17; Z[i]=s;}
  // caps for first-guess classes
  int caps[P+1]; {int b=1; for(int r=P;r>=0;r--){ } }
  for(int r=0;r<=P;r++){ long cnt=0; for(int i=0;i<NC;i++) if(fb(0,i)==r) cnt++; caps[r]=cnt; }
  G[0]=0;G[1]=1; for(int k=2;k<64;k++){long v=1; for(int r=0;r<P;r++) v+= caps[r]<G[k-1]?caps[r]:G[k-1]; G[k]=v>1000000000?1000000000:v;}
  code *S=malloc(sizeof(code)*NC); int n=0;
  if(which<0){ for(int i=0;i<NC;i++) S[n++]=i; }
  else { for(int i=0;i<NC;i++) if(fb(0,i)==which) S[n++]=i; hist[nh++]=0; }
  for(int d=1; d<=dmax; d++){
    int r=solve(S,n,d);
    printf("C=%d branch=%d n=%d d=%d : %s nodes=%ld hfull=%ld\n",C,which,n,d,r?"YES":"no",nodes,hfull); fflush(stdout);
    if(r) break;
  }
}
