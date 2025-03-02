public class NeVector<Item,Len extends Int> extends Vector<Item,Succ<Len>> {
  protected final Item head;
  protected final Vector<Item,Len> tail;

  public NeVector(final Item item, final Vector<Item,Len> stack) {
    head = item; tail = stack; }

  public Item head() { return head; }
  public Vector<Item,Len> tail() { return tail; }

  public void print() { System.out.print(head + " "); tail.print(); }
}
