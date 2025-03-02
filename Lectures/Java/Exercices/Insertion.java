public class Insertion {
  static public void main (String[] args) {
    int n = args.length;
    int[] a = new int[n];
    for (int i = 0; i < n; i++) {
      a[i] = Integer.parseInt(args[i]);
    }
    int key = -1;
    int i = -1;
    for (int j = 1; j < n; j++) {
      key = a[j];
      i = j - 1;
      while (i >= 0 && a[i] > key) {
        a[i+1] = a[i];
        i = i - 1;
      }
      a[i+1] = key;
    }
    for (i = 0; i < n; i++) {
     System.out.print (a[i] + " "); 
    }
    System.out.println();
  }
}
