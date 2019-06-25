#define N 5

bit fork[N];


init {
  int i = 0;
  do
  :: i < N -> atomic { 
                printf ("Philosopher %d takes a seat.\n", i);
                run philosopher(i); 
                i = i + 1
              }
  :: i == N -> break
  od
}


#define leftFork(p) p % N
#define rightFork(p) (p + 1) % N


inline pickUpLeft(p) {
  atomic {
    fork[leftFork(p)] == 0
    fork[leftFork(p)] = 1
  }
}

inline pickUpRight(p) {
  atomic {
    fork[rightFork(p)] == 0
    fork[rightFork(p)] = 1
  }
}

inline putDownLeft(p) {
  fork[leftFork(p)] = 0
}

inline putDownRight(p) {
  fork[rightFork(p)] = 0
}


inline think(p) {
  printf("Philosopher %d is thinking.\n", p)
}
	
inline eat(p) {
  printf("Philosopher %d is eating.\n", p)
}


proctype philosopher(int p) {
  do
  :: think(p) /* philosopher decides to think */
  :: /* philosopher decides to eat */
     /* pick up forks */
     if
     :: leftFork(p) < rightFork(p) --> pickUpLeft(p); pickUpRight(p);
     :: leftFork(p) > rightFork(p) --> pickUpRight(p); pickUpLeft(p);
     fi
     /* ... complete ... */

     eat(p);  /* eventually eat some spaghetti from the platter */

     /* put down forks */
     if
     :: putDownLeft(p); putDownRight(p);
     :: putDownRight(p); putDownLeft(p);
     fi
     /* ... complete ... */
  od
}


