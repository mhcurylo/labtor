import io.vavr.Function1;
import io.vavr.collection.Map;
import io.vavr.collection.Stream;
import io.vavr.collection.TreeMap;

import java.util.Arrays;

public class CountWords {

    private static final Function1<String, String[]> toArrayOfTokens = txt -> txt
            .toLowerCase()
            .replaceAll("[^a-zA-Z\\d\\s:]", "")
            .split("\\s+");

    static final Function1<String, Stream<String>> listTokens = toArrayOfTokens
            .andThen(Arrays::stream)
            .andThen(Stream::ofAll)
            .andThen(s -> s.filter(x -> !x.equals("")));

    static final Function1<Stream<String>, Map<String, Integer>> countTokens = stream -> stream
            .foldLeft(TreeMap.empty(), (p, c) -> p.put(c, p.getOrElse(c, 0) + 1));

    public static final Function1<String, Map<String, Integer>> countWords = countTokens.compose(listTokens);
}
