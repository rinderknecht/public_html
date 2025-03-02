public class EphObj {
  public static void main (String[] args) {
    Stack<Integer> s = new Stack<Integer>();
    s.push(5); s.push(3); s.push(1); s.push(0);
    s.print(); // 0 1 3 5
    try { System.out.println(s.find_front(5)); } // 3
    catch (NotFound x) {}
    s.print(); // 5 0 1 3
    try { System.out.println(s.find_front(5)); } // 0
    catch (NotFound x) {}
    s.print(); // 5 0 1 3
    try { System.out.println(s.find_front(1)); } // 2
    catch (NotFound x) {}
    s.print(); // 1 5 0 3
    try { System.out.println(s.find_front(7)); }
    catch (NotFound x) { System.out.println("Not found."); }

    try { System.out.println(s.find_swap(3)); } // 3
    catch (NotFound x) {}
    s.print(); // 1 5 3 0
    try { System.out.println(s.find_swap(3)); } // 2
    catch (NotFound x) {}
    s.print(); // 1 3 5 0
    try { System.out.println(s.find_swap(1)); } // 0
    catch (NotFound x) {}
    s.print(); // 1 3 5 0
    try { System.out.println(s.find_swap(7)); }
    catch (NotFound x) { System.out.println("Not found."); }

    try { System.out.println(s.find_swap(3)); }
    catch (NotFound x) {}
    s.print(); // 3 1 5 0
  }
}
