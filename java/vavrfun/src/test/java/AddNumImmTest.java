import io.vavr.CheckedFunction2;
import io.vavr.CheckedFunction4;
import io.vavr.Function1;
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
    public void composes() {
        CheckedFunction4<Integer, Integer, Integer, List<Integer>, Boolean> composed = (num1, num2, num3, ls) -> {
            Function1<List<Integer>, List<Integer>> add1 = AddNumImm.addNum.apply(num1);
            Function1<List<Integer>, List<Integer>> add2 = AddNumImm.addNum.apply(num2);
            Function1<List<Integer>, List<Integer>> add3 = AddNumImm.addNum.apply(num3);

            Function1<List<Integer>, List<Integer>> add1234 = add1.compose(add2).compose(add3);

            return add1234.apply(ls).equals(add1.apply(add2.apply(add3.apply(ls))));
        };

        CheckResult result = Property
                .def("Adds one element to array")
                .forAll(Arbitrary.integer(), Arbitrary.integer(), Arbitrary.integer(), Arbitrary.list(Arbitrary.integer()))
                .suchThat(composed)
                .check();

        result.assertIsSatisfied();
    }

    @Test
    public void doesNotUseInputAsOutput() {
        CheckedFunction2<Integer, List<Integer>, Boolean> outputNotChanged = (num, ls) -> AddNumImm.addNum.apply(num).apply(ls).size() > ls.size();

        CheckResult result = Property
                .def("Does not output on input")
                .forAll(Arbitrary.integer(), Arbitrary.list(Arbitrary.integer()))
                .suchThat(outputNotChanged)
                .check();

        result.assertIsSatisfied();
    }

}