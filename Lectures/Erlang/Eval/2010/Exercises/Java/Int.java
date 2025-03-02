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

  protected Stack<Item> inorder(final Stack<Item> s) {
    return left.inorder(right.inorder(s).push(root));
  }

  public void print () { 
    left.print(); System.out.print(root + " "); right.print(); }
}
