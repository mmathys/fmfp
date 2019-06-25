#define N 5 /* number of processes in the ring */
#define L 1 /* length of a channel */
chan c[N] = [L] of { byte };

proctype pnode(chan _in, out; byte id) {
  out ! id
  do
  ::
    byte m
    _in ? m
    m == id -> break
    m > id -> out ! id
  od
}

init {
  int i = 0;
  do
  :: i < N -> atomic { 
                printf ("Start process %d\n", i);
                run pnode(c[i], c[(i+1) % N], i);
                i = i + 1
              }
  :: i == N -> break
  od
}