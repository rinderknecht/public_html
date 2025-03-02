public class Ext<Item extends Comparable<? super Item>> extends BST<Item> {
  public Int<Item> add(final Item item) {
    return new Int<Item>(item,this,this); }

  public int size() { return 0; }
  public void inorder() {}
  public int height () { return 0; }
  public Item max() throws EmptyBST { throw new EmptyBST(); }
  public Item max_aux(final Item parent) throws EmptyBST { return parent; }
  public EStack<Item> to_stack() { return new EStack<Item>(); }
  protected Stack<Item> to_stack_aux(final Stack<Item> stack) {
    return stack; }
}
