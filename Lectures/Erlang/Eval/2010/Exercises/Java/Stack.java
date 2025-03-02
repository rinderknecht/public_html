public abstract class Stack<Item extends Comparable<? super Item>> {
  public final NStack<Item> push(final Item item) {
    return new NStack<Item>(item,this); }
  public    abstract Stack<Item>  cat(final Stack<Item> stack);
  public    abstract void print();
  public             Stack<Item>  rev() { return rev_cat(new EStack<Item>()); }
  public    abstract Stack<Item>  rev_cat(final Stack<Item> stack);
  public    abstract Stack<Item>  rm_fst(final Item item);
  public    abstract Stack<Item>  rm_lst(final Item item);
  protected abstract Stack<Item>  rm_lst_aux(final Item item, 
                                             final Stack<Item> orig);
  public    abstract Stack<Item>  isort();
  protected abstract NStack<Item> insert(final Item item);
  public    abstract Pair<Stack<Item>,Stack<Item>> split(final int k);
  public    abstract Stack<Item>  mrg(final Stack<Item> t);
  public    abstract Stack<Item>  mrg0(final Item x,final Stack<Item> s);
  public    abstract Stack<Item> tms();
  protected abstract Stack<Item> tms0(final Item x);
  public    abstract Stack<Item> cutr(final Stack<Item> s,
                                      final Stack<Item> u);
  protected abstract Stack<Item> cutr0(final Stack<Item> s,
                                       final Stack<Item> t);
  protected abstract Stack<Item> cutr1(final Stack<Item> s,
                                       final Stack<Item> t);
}
