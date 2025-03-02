public abstract class Stack<Item extends Comparable<? super Item>> {
  public NStack<Item> push(final Item item) {
    return new NStack<Item>(item,this); }
  public    abstract Stack<Item> rev();
  public    abstract Stack<Item> rev_join(final Stack<Item> stack);
  public    abstract Stack<Item> join(final Stack<Item> stack);
  public    abstract Stack<Item> rm_bot();
  protected abstract Stack<Item> rm_bot_aux(final Item item);
  public    abstract Stack<Item> rm_fst(final Item item);
  public    abstract Stack<Item> rm_lst(final Item item);
  protected abstract Stack<Item> rm_lst_aux(final Item item, 
                                            final Stack<Item> orig);
  public    abstract Stack<Item> shuffle(final Stack<Item> stack);
  public    abstract Stack<Item> rep_fst();
  protected abstract Stack<Item> rep_fst_aux(final Item item);
  public    abstract Stack<Item> isort();
  protected abstract NStack<Item> insert(final Item item);
  public    abstract Stack<Item> merge(final Stack<Item> stack);
  protected abstract Stack<Item> merge_aux(final Item item,
                                           final Stack<Item> stack,
                                           final NStack<Item> orig);
  public    abstract BST<Item> to_BST();
  public    abstract void print();
}
