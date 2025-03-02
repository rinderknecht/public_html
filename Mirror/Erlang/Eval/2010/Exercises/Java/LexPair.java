public class LexPair<Fst extends Comparable<Fst>,
                     Snd extends Comparable<Snd>>
       implements Comparable<LexPair<Fst,Snd>> {
  protected final Fst fst;
  protected final Snd snd;

  public LexPair(final Fst f, final Snd s) { fst = f; snd = s; }

  public Fst fst() { return fst; }
  public Snd snd() { return snd; }

  public int compareTo(final LexPair<Fst,Snd> p) {
    int cmp_fst = fst.compareTo(p.fst());
    return cmp_fst == 0 ? snd.compareTo(p.snd()) : cmp_fst;
  }
}
