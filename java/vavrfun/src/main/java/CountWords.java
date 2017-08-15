import io.vavr.Function1;
import io.vavr.Tuple2;
import io.vavr.collection.List;
import io.vavr.collection.Map;
import io.vavr.collection.Stream;
import io.vavr.collection.TreeMap;
import io.vavr.control.Either;
import io.vavr.control.Try;

import java.nio.file.Files;
import java.nio.file.Paths;

public class CountWords {

    private static final Function1<String, String[]> toArrayOfTokens = txt -> txt
            .toLowerCase()
            .replaceAll("[^a-zA-Z\\d\\s:]", "")
            .split("\\s+");

    static final Function1<String, Stream<String>> listTokens = toArrayOfTokens
            .andThen(Stream::of)
            .andThen(s -> s.filter(x -> !x.equals("")));

    static final Function1<Stream<String>, Map<String, Integer>> countTokens = stream -> stream
            .foldLeft(TreeMap.empty(), (p, c) -> p.put(c, p.getOrElse(c, 0) + 1));

    public static final Function1<String, Map<String, Integer>> countWords = countTokens.compose(listTokens);

    static final Function1<Map<String, Integer>, List<Tuple2<String, Integer>>> wordsByCount = tree -> tree
            .toList()
            .sortBy(Tuple2::_2);

    public static void main (String[] args) {
        Either<String, List<Tuple2<String, Integer>>> wordCount = Either.<String, String[]>right(args)
                .flatMap(CountWords::safeArgHead)
                .flatMap(CountWords::safeFileToLines)
                .map(stream -> stream.flatMap(listTokens))
                .map(countTokens)
                .map(wordsByCount)
                .mapLeft(err -> "Error. \n" + err + "\n");

        wordCount.peek(System.out::print);

        wordCount.orElseRun(System.out::print);
    }

    private static Either<String, String> safeArgHead(String[] args) {
        return Try.of(() -> args[0])
                .toEither("No argument supplied.");
    }

    private static Either<String, Stream<String>> safeFileToLines(String filename) {
        return Try.of(() -> Stream.ofAll(Files.lines(Paths.get(filename))))
                .toEither("File does not exist.");
    }

}
