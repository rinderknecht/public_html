public class EStack<Item extends Comparable<? super Item>> 
       extends Stack<Item> {
  public EStack<Item> rev() { return this; }

  public Stack<Item> rev_join(final Stack<Item> stack) { 
    return stack; }

  public Stack<Item> join(final Stack<Item> stack) {
    return stack; }

  public EStack<Item> rm_bot() { return this; }

  protected EStack<Item> rm_bot_aux (final Item item) { 
    return this; }

  public EStack<Item> rm_fst(final Item item) {
    return this; }

  public EStack<Item> rm_lst(final Item item) {
    return this; }

  protected Stack<Item> rm_lst_aux(final Item item, 
                                   final Stack<Item> orig) {
    return orig;
  }
 
  public Stack<Item> shuffle(final Stack<Item> stack) {
    return stack; }

  public EStack<Item> rep_fst() {
    return this; }

  protected Stack<Item> rep_fst_aux(final Item item) {
    return this; }

  public EStack<Item> isort() {
    return this; }

  protected NStack<Item> insert(final Item item) {
    return push(item); }

  public Stack<Item> merge(final Stack<Item> stack) {
    return stack; }

  protected NStack<Item> merge_aux(final Item item,
                                   final Stack<Item> stack,
                                   final NStack<Item> orig) {
    return orig; }

  public Ext<Item> to_BST() { return new Ext<Item>(); }

  public void print() { System.out.println(); }
}
