public class Selection {
  static public void main (String[] args) {
    int n = args.length;
    int[] a = new int[n];
    for (int i = 0; i < n; i++) {
      a[i] = Integer.parseInt(args[i]);
    }
    int m = -1;
    for (int i = 0; i <= n-2; i++) {
      m = i;
      for (int j = i+1; j <= n-1; j++)
	  if (a[j] < a[m]) m = j;
      int t = a[m];
      a[m] = a[i];
      a[i] = t;
    }
    for (int i = 0; i < n; i++) {
     System.out.print (a[i] + " "); 
    }
    System.out.println();
  }
}
