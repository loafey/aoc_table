typedef char TABLE_GEN;

TABLE_GEN *create_table(char *ptr, long len);

void run(TABLE_GEN *ptr);

void add_func(TABLE_GEN *ptr, void (*p1)(), void (*p2)());