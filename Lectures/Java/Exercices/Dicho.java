public class Dicho {

    public static boolean lookup (int n, int[] t, int low, int high) {
        if (low >= t.length || high >= t.length || low > high) 
          return false;
        int middle = (low + high) / 2;
        if (t[middle] < n) return lookup(n, t, middle + 1, high);
        if (t[middle] > n) return lookup(n, t, low, middle - 1);
        return true;
    }

    public static void print (int[] t) { 
        int i = 0;
	for (i = 0; i < t.length; i++)
          System.out.print(t[i] + " ");
        System.out.println();
    }

    public static void main (String[] args) {
        if (args.length == 1) { 
	  int n = Integer.parseInt(args[0]);
          int[] t = {3,7,8,13,15,100};
          print(t);
          if (lookup(n,t,0,t.length-1))
	    System.out.println("Pr�sent.");
          else System.out.println("Absent.");
        }
        else System.out.println("usage: java Dicho <num>");
    }

}
