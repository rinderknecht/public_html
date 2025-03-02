public class EStack<Item extends Comparable<? super Item>> 
       extends Stack<Item> {
  public Stack<Item> cat(final Stack<Item> stack) {
    return stack; }

  public Stack<Item> rev_cat(final Stack<Item> stack) { 
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

  public EStack<Item> isort() {
    return this; }

  protected NStack<Item> insert(final Item item) {
    return push(item); }

  public void print() { System.out.println(); }
}
