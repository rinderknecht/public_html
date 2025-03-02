public class Ext<Item extends Comparable<? super Item>> extends BST<Item> {
  public Int<Item> add(final Item item) {
    return new Int<Item>(item,this,this); }

  public void print() {}
}
