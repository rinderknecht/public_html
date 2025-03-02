public class EStack<Item extends Comparable<? super Item>> 
       extends Stack<Item> {
  public Stack<Item> cat(final Stack<Item> stack) {
    return stack; }

  public void print() { System.out.println(); }

  public Stack<Item> rev_cat(final Stack<Item> stack) { 
    return stack; }

  public EStack<Item> rm_fst(final Item item) {
    return this; }

  public EStack<Item> rm_lst(final Item item) {
    return this; }

  protected Stack<Item> rm_lst_aux(final Item item, 
                                   final Stack<Item> orig) {
    return orig; }
 
  public EStack<Item> isort() { return this; }

  protected NStack<Item> insert(final Item item) {
    return push(item); }

  public Pair<Stack<Item>,Stack<Item>> split (final int k) {
    return new Pair<Stack<Item>,Stack<Item>>(this,this); }

  public Stack<Item> mrg(final Stack<Item> t) { return t; }

  public Stack<Item> mrg0(final Item x,final Stack<Item> s) {
    return s.push(x); }

  public Stack<Item> tms() { return this; }

  protected Stack<Item> tms0(final Item x) { return push(x); }

  public Stack<Item> cutr(final Stack<Item> s,
                          final Stack<Item> t) {
    return s.tms().mrg(t.tms()); }

  protected Stack<Item> cutr0(final Stack<Item> s,
                              final Stack<Item> t) {
    return s.tms().mrg(t.tms()); }

  protected Stack<Item> cutr1(final Stack<Item> s,
                              final Stack<Item> u) {
    return s.tms(); }
}
