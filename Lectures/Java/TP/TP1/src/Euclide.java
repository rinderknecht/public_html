public class Euclide {

  public static int pgcd (int a, int b) {
    if (b > a) 
    {
     int c = a;
     a = b;
     b = c;
    }
    int r = 0;
    while (b != 0) {
      r = a % b;
      a = b;
      b = r;
    }
    return a;
  }

  public static int rec_pgcd (int a, int b) {
      if (b > a)
        return rec_pgcd (b,a);
      else
	  if (b == 0)
            return a;
          else
            return rec_pgcd (b, a%b);
  }

  public static void main (String[] args) {
      if (args.length == 2) {
        int a = Integer.parseInt (args[0]);
        int b = Integer.parseInt (args[1]);
        System.out.println ("pgcd(" + a + "," + b + ") = " + pgcd(a,b));
        System.out.println ("rec_pgcd(" + a + "," + b + ") = " + rec_pgcd(a,b));
      }
      else
        System.out.println ("Usage: java Euclide <num> <num>");
  }
}
