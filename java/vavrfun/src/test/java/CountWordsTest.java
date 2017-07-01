import io.vavr.CheckedFunction1;
import io.vavr.Function1;
import io.vavr.collection.List;
import io.vavr.collection.Map;
import io.vavr.test.Arbitrary;
import io.vavr.test.CheckResult;
import io.vavr.test.Gen;
import io.vavr.test.Property;
import org.junit.Test;
import java.util.function.Predicate;


public class CountWordsTest {
    private static final Arbitrary<List<String>> listArbitrary = Arbitrary.list(Arbitrary.string(Gen.choose('a', 'z')));
    private static final Function1<List<String>, String> listToString = ls -> String.join(" ", ls.asJava());
    private static final Predicate<String> isNotEmpty = x -> !x.equals("");
    private static final Function1<String, Predicate<String>> isEqual = s -> x -> x.equals(s);

    @Test
    public void tokenizes() {
        CheckedFunction1<List<String>, Boolean> splitsItRight = ls -> {
            List<String> fls = ls.filter(isNotEmpty);

            return CountWords.listTokens.apply(listToString.apply(fls)).size() == fls.size();
        };

        CheckResult result = Property
                .def("Tokenizes the string")
                .forAll(listArbitrary)
                .suchThat(splitsItRight)
                .check();

        result.assertIsSatisfied();
    }

    @Test
    public void countsEverything() {
        CheckedFunction1<List<String>, Boolean> allAreCounted = ls -> {
            Map<String, Integer> count = CountWords.countTokens.apply(ls.toStream());

            return ls.foldLeft(true, (p, c) -> p && (ls.filter(isEqual.apply(c)).size() == count.getOrElse(c, 0)));
        };

        CheckResult result = Property
                .def("Counts tokens properly")
                .forAll(listArbitrary)
                .suchThat(allAreCounted)
                .check();

        result.assertIsSatisfied();
    }

    @Test
    public void countsWords() {
        CheckedFunction1<List<String>, Boolean> allAreCounted = ls -> {
            List<String> fls = ls.filter(isNotEmpty);
            Map<String, Integer> count = CountWords.countWords.apply(listToString.apply(fls));

            return ls.foldLeft(true, (p, c) -> p && (fls.filter(isEqual.apply(c)).size() == count.getOrElse(c, 0)));
        };

        CheckResult result = Property
                .def("Counts words properly")
                .forAll(listArbitrary)
                .suchThat(allAreCounted)
                .check();

        result.assertIsSatisfied();
    }

}