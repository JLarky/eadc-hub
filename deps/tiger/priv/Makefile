#On all machines with gcc (including Alpha)
CC = gcc -O3
#On 32-bit machines
#CC = cc -O4
#On Alpha:
#CC = cc -O -migrate


#tiger_drv: tiger.dll
tiger: tiger_drv.c tiger.c
	$(CC) -o tiger_drv.so -fpic -shared tiger_drv.c tiger.c

tiger-win: tiger_drv.c tiger.c
	$(CC) -o tiger_drv.dll -fpic -shared tiger_drv.c tiger.c

clean:
	$(RM) tiger_drv.so tiger_drv.dll