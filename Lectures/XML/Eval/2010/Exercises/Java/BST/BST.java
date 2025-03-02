public abstract class BST<Item extends Comparable<? super Item>> {
  public    abstract Int<Item> add(final Item item);
  public    abstract int size();
  public    abstract void inorder();
  public    abstract int height();
  public    abstract Item max() throws EmptyBST;
  protected abstract Item max_aux(final Item parent) throws EmptyBST;
  public    abstract Stack<Item> to_stack();
  protected abstract Stack<Item> to_stack_aux(final Stack<Item> stack);
}
