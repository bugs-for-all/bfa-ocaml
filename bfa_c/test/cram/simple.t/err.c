void *malloc(int);

int main()
{
  int *x = malloc(1024);
  *x = 12;

  return 0;
}