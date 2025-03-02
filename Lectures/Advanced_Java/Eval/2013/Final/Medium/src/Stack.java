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

    public void rm_fst (final Key k) {
      Cell<Key> index = new Cell<Key>(null,top);
      while (index.next != null && index.next.value.compareTo(k) != 0)
        index = index.next;
      if (index.next != null)
        if (index.next == top) top = top.next;
        else index.next = index.next.next;
    }

    public void rm_lst (final Key k) {
      Cell<Key> index = new Cell<Key>(null,top);
      Cell<Key> last = null;
      while (index.next != null) {
        if (index.next.value.compareTo(k) == 0)
          last = index;
        index = index.next;
      }
      if (last != null)
        if (last.next == top) top = top.next;
        else if (last.next != null) last.next = last.next.next;
    }

}
