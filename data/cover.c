/* MDH WCET BENCHMARK SUITE. File version $Id:  Exp $ */

/* Changes:
 * JG 2005/12/21: Inserted function prototypes
                  Indented program.
 */

int             swi2(int c);

int
swi2(int c)
{
	int             i;
	for (i = 0; i < 3; i++) {
		switch (i) {
		case 0:
			c++;
			break;
		case 1:
			c++;
			break;
		case 2:
			c++;
			break;
		}
        }
	return c;
}

int
main()
{
	volatile int    cnt = 0;
	cnt = swi2(cnt);
	/* printf("cnt: %d\n", cnt); */
	return cnt;

}
