public class Stack<Key extends Comparable<Key>> {
    protected Cell<Key> top;

    public Stack () { top = null; }
 
    protected Stack (final Cell<Key> c) { top = c; }

    public void push (final Key k) {
      top = new Cell<Key>(k,top);
    }

    public void print () {
      if (top != null) { top.print(); System.out.println(); }
    }

    public int find_front (final Key k) throws NotFound {
      Cell<Key> index = new Cell<Key>(null,top);
      int n = 0;
      while (index.next != null && index.next.value.compareTo(k) != 0) {
        index = index.next; n++; }
      if (index.next == null) throw new NotFound ();
      if (index.next != top) { // Move to front
        Cell<Key> tem = index.next;
        index.next = tem.next;
        tem.next = top;
        top = tem;
      }
      return n;
    }

    public int find_swap (final Key k) throws NotFound {
      Cell<Key> index = 
        new Cell<Key>(null,new Cell<Key>(null,top));
      int n = 0;
      while (index.next.next != null
             && index.next.next.value.compareTo(k) != 0) {
        index = index.next; n++; }
      if (index.next.next == null) throw new NotFound ();
      if (index.next.next != top) { // Swap previous
        Cell<Key> tem = index.next.next;
        index.next.next = tem.next;
        tem.next = index.next;
        index.next = tem;
        if (top == tem.next) top = tem;
      }
      return n;
    }

}
