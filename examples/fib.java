public class fib {
    static public void main(String[] args) {
        int n = Integer.parseInt(args[0]);
        int k = 10;
        double sum = 0.0;
        fib f = new fib();
        Object env = "dummy-env";
        for (int i = 0; i < k; i++) {
            int m;
            long t;
            t= System.currentTimeMillis();
            m = f.fib(n, env);
            t = System.currentTimeMillis() - t;
            System.out.println("fib("+n+")="+m+" elapsed time: "+t+"[msec]");
            sum += t;
        }
        System.out.println("avg: "+((long) (sum/k))+"[msec]");
    }
    public int fib(int n, Object env) {
        if (n < 2)
            return n;
        else
            return fib(n-1, env)+fib(n-2, env);
    }

    //public int fib(int n, Object env) {
    //    int x = n;
    //    if (lt(x,2))
    //        return x;
    //    else
    //        return add(fib(sub(x,1), env),fib(sub(x,2), env));
    //}
    //static public int add(int i, int j) {
    //    return i+j;
    //}
    //static public int sub(int i, int j) {
    //    return i-j;
    //}
    //static public boolean lt(int i, int j) {
    //    return i<j;
    //}

    //static public int fib(int n) {
    //    if (n < 2)
    //        return n;
    //    else
    //        return fib(n-1)+fib(n-2);
    //}
    //
    //public static int fib(int);
    //  Code:
    //   0:	iload_0
    //   1:	iconst_2
    //   2:	if_icmpge	7
    //   5:	iload_0
    //   6:	ireturn
    //   7:	iload_0
    //   8:	iconst_1
    //   9:	isub
    //   10:	invokestatic	#4; //Method fib:(I)I
    //   13:	iload_0
    //   14:	iconst_2
    //   15:	isub
    //   16:	invokestatic	#4; //Method fib:(I)I
    //   19:	iadd
    //   20:	ireturn

}
