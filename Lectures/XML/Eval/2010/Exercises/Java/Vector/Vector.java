public abstract class Vector<Item,Len extends Int> {
  public Vector<Item,Succ<Len>> push(final Item item) {
    return new NeVector<Item,Len>(item,this); }
  public abstract void print ();
}
