// This program offers two implementations of binary search on an
// increasingly sorted array of integers. Amongst the many possible
// designs, one uses a loop and an exception, whilst the other uses
// recursion and an exception. Other possibilities would include using
// a negative index as an error code, instead of an exception, or not
// exiting immediately upon finding the key but use one comparison
// instead of two per cycle, etc. Note that we avoid the common
// potential arithmetic overflow due to (low + high)/2.

public class BinSearch {

  static class NotFound extends Exception {}

  public static int search(int[] a, int key) throws NotFound {
    int low = 0;
    int high = a.length - 1;
    while (low <= high) {
      int mid = low + (high - low)/2; // NOT (low + high) / 2;
      int midVal = a[mid];
      if (midVal < key) low = mid + 1;
      else if (midVal > key) high = mid - 1;
           else return mid; // key found
    }
    throw new NotFound();
  }

  public static int rec_search(int[] a, int key, int low, int range)
    throws NotFound {
      if (range == 0 || low == a.length) throw new NotFound();
      int half = range/2;
      int mid = low + half;
      int midVal = a[mid];
      if (midVal < key) return rec_search(a,key,mid+1,half);
      else if (midVal > key) return rec_search(a,key,low,half);
      return mid; // key found
  }

  public static void main (String[] args) {
    int[] a = new int[] {1,4,7,8,9};
    int k = Integer.parseInt(args[0]);
    try { System.out.print(rec_search(a,k,0,a.length)); }
    catch (NotFound x) { System.out.print("Not found."); }
    System.out.println();
  }
}
