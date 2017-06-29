import io.vavr.Function1;
import io.vavr.collection.List;

public class AddNumImm {
    public static Function1<Integer, Function1<List<Integer>, List<Integer>>> addNum = (num -> ls -> ls.prepend(num));
}
