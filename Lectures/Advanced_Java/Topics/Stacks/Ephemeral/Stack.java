public class Stack<Key extends Comparable<Key>> {
    protected Cell<Key> top;

    public Stack () { top = null; }
 
    protected Stack (final Cell<Key> c) { top = c; }

    public void push (final Key k) {
      top = new Cell<Key>(k,top);
    }

    public boolean isEmpty () { return top == null; }

    public void print () {
      if (top != null) { top.print(); System.out.println(); }
    }

    public void cat (final Stack<Key> s) {
      if (top == null) top = s.top;
      else {
        Cell<Key> index = top;
        while (index.next != null) index = index.next;
        index.next = s.top;
        s.top = null;
      }
    }

    public void rev_cat (final Stack<Key> s) {
      while (s.top != null) {
        Cell<Key> index = s.top;
        s.top = index.next;
        index.next = top;
        top = index;
      }
    }

    public void rev () {
      Stack<Key> s = new Stack<Key>();
      s.rev_cat(this);
      top = s.top;
    }

    public void isort () {
      Cell<Key> sentinel = new Cell<Key>(null,null);

      while (top != null) {
        Cell<Key> index = sentinel;

        while (index.next != null 
               && index.next.value.compareTo(top.value) < 0)
          index = index.next;

        Cell<Key> tmp = index.next;
        index.next = top;
        top = top.next;
        index.next.next = tmp;
      }

      top = sentinel.next;
    }

  void merge (final Stack<Key> s) {
    Cell<Key> sentinel = new Cell<Key>(null,s.top);
    Cell<Key> index = sentinel;

    while (top != null) {
      while (index.next != null 
             && index.next.value.compareTo(top.value) < 0)
        index = index.next;

      Cell<Key> tmp = index.next;
      index.next = top;
      top = top.next;
      index = index.next;
      index.next = tmp;
    }

    top = sentinel.next;
    s.top = null;
  } 

  void merge_opt (final Stack<Key> s) {
    Cell<Key> sentinel = new Cell<Key>(null,s.top);
    Cell<Key> index = sentinel;

    while (top != null) {
      while (index.next != null 
             && index.next.value.compareTo(top.value) < 0)
        index = index.next;

      Cell<Key> tmp = index.next;
      index.next = top;
      top = tmp;
      index = index.next;
    }

    top = sentinel.next;
    s.top = null;
  } 

  public Stack<Key> split () {
    Cell<Key> one = top;
    Cell<Key> two = top;

    while (two.next != null && two.next.next != null) {
      one = one.next;
      two = two.next.next;
    }
    
    two = one.next;
    one.next = null;
    return new Stack<Key>(two);
  }

  public void msort () {
    if (top != null && top.next != null) {
      Stack<Key> s = this.split();
      this.msort();
      s.msort();
      this.merge_opt(s);
    }
  }

}
