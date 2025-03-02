import java.lang.*;

public class Binary {

  public static boolean[] convert (int n) { 
    int size = 1;
    if (n != 0)
      size = 1 + (int) Math.floor (Math.log(n) / Math.log(2));
    boolean[] bin = new boolean [size];
    int i = 0;
    int remainder = 0;
    for (i = 0; i < size; i++) {
      remainder = n % 2;
      if (remainder == 0) 
        bin[i] = false; 
      else 
        bin[i] = true;
      n = n / 2;
    }
    return bin;
  }

  public static void print (boolean[] b) {
    int index = 0;
    for (index = b.length - 1; index >= 0; index--) {
      if (b[index])
        System.out.print (1);
      else
        System.out.print (0);
    }
    System.out.println();
  }

  public static void main (String[] args) {
    if (args.length == 1)
    {
     int n = Integer.parseInt (args[0]);
     boolean[] bin = convert(n);
     print (bin);
    }
    else
      System.out.println ("Usage: java Binary <num>");
  }
}
