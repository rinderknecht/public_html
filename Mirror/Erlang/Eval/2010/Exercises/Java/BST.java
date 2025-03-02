public abstract class BST<Item extends Comparable<? super Item>> {
  public    abstract Int<Item>   add(final Item item);
  public    Stack<Item> inorder() { return inorder(new EStack<Item>()); }
  protected abstract Stack<Item> inorder(final Stack<Item> s);
  public    abstract void        print();
}
