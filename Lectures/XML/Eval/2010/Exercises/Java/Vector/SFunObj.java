public class SFunObj {
  public static void main (String[] args) {
    Vector<Integer,Succ<Succ<Zero>>> V = 
        (new EVector<Integer,Zero>()).push(3).push(2);
    V.print();
  }
}
