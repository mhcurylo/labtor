import io.vavr.CheckedFunction2;
import io.vavr.collection.List;
import io.vavr.test.Arbitrary;
import io.vavr.test.CheckResult;
import io.vavr.test.Property;
import org.junit.Test;

public class AddNumImmTest {
    @Test
    public void addsNum() {
        CheckedFunction2<Integer, List<Integer>, Boolean> numAdded = (num, ls) -> AddNumImm.addNum
                .apply(num)
                .apply(ls)
                .get(0)
                .equals(num);

        CheckResult result = Property
                .def("Adds one element to array")
                .forAll(Arbitrary.integer(), Arbitrary.list(Arbitrary.integer()))
                .suchThat(numAdded)
                .check();

        result.assertIsSatisfied();
    }

    @Test
    public void doesNotOutputOnInput() {
        CheckedFunction2<Integer, List<Integer>, Boolean> outputNotChanged = (num, ls) -> AddNumImm.addNum.apply(num).apply(ls).size() > ls.size();

        CheckResult result = Property
                .def("Does not output on input")
                .forAll(Arbitrary.integer(), Arbitrary.list(Arbitrary.integer()))
                .suchThat(outputNotChanged)
                .check();

        result.assertIsSatisfied();
    }

}