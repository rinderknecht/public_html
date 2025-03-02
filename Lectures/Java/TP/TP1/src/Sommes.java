public class Sommes {
  
  public static int carres (int n) {
    int i = 0;
    int r = 0;
    for (i = 0; i <= n; i++) {
      r = r + i*i;
    }
    return r;
  }

  public static int rec_carres (int n) {
    if (n == 0)
      return 0;
    else return n*n + rec_carres (n-1);
  }

  public static void main (String[] args) {
      if (args.length == 1) {
        int n = Integer.parseInt (args[0]);
        System.out.println ("carres(" + n + ") = " + carres(n));
        System.out.println ("rec_carres(" + n + ") = " + rec_carres(n)); 
      }
      else
        System.out.println ("Usage: java Sommes <num>");
    
  }
}
