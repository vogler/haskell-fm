int myglobal;
int mutex = 0;

void munge(int *m) {
  lock(m);
  myglobal=myglobal+1;
  unlock(m);
}

void t_fun() {
  munge(&mutex);
}


int main(void) {
  spawn(t_fun);
  munge(&mutex);
  return 0;
}
