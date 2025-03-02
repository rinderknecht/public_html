public class Int<Item extends Comparable<? super Item>> extends BST<Item> {
  protected final Item root;
  protected final BST<Item> left, right;

  public Int(final Item i, final BST<Item> l, final BST<Item> r) {
    root = i; left = l; right = r; }

  public Int<Item> add(final Item item) {
    return item.compareTo(root) <= 0 ?
           new Int<Item>(root,left.add(item),right)
         : new Int<Item>(root,left,right.add(item));
  }

  public int size() { return 1 + left.size() + right.size(); }

  public void inorder () { 
    left.inorder(); System.out.print(root + " "); right.inorder(); }

  public int height () {
    final int lh = left.height();
    final int rh = right.height();
    return 1 + (lh > rh ? lh : rh);
  }

  public Item max() throws EmptyBST { return right.max_aux(root); }

  public Item max_aux(final Item parent) throws EmptyBST { return max(); }

  public Stack<Item> to_stack() {
    return to_stack_aux(new EStack<Item>()); }

  protected Stack<Item> to_stack_aux(final Stack<Item> stack) {
    return left.to_stack_aux(right.to_stack_aux(stack).push(root));
  }
}
