public class Bubble {

  public static void sort (int[] t) {
    int i = 0;
    int j = 0;
    for (i = t.length - 1; i >= 0; i--) {
      for (j = 1; j <= i; j++) {
	  if (t[j-1] > t[j]) {
	   int tmp = t[j-1];
           t[j-1] = t[j];
           t[j] = tmp;
          }
      }
    }      
  }

  public static void print (int[] t) {
    int i = 0;
    for (i = 0; i < t.length; i++)
      System.out.print (t[i] + " ");
    System.out.println ();
  }

  public static void main (String[] args) {
   int i = 0;
   int[] t = new int [args.length];
   for (i = 0; i < args.length; i++)
     t[i] = Integer.parseInt (args[i]);

   print (t);
   sort (t);
   print (t);
  }
}
