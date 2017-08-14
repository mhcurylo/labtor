import io.vavr.CheckedFunction1;
import io.vavr.test.CheckResult;
import io.vavr.test.Property;
import org.junit.Test;
import io.vavr.test.Arbitrary;

import java.util.ArrayList;
import java.util.List;

public class AddNumTest {

    @Test
    public void addsNum() {
        List<Integer> list = new ArrayList<>();
        list.add(0);

        CheckedFunction1<Integer, Boolean> numAdded = x -> new AddNum(x).to(list).get(0).equals(x);

        CheckResult result = Property
                .def("Adds one element to array")
                .forAll(Arbitrary.integer())
                .suchThat(numAdded)
                .check();

        result.assertIsSatisfied();
    }

    @Test
    public void doesNotUseInputAsOutput() {

        CheckedFunction1<Integer, Boolean> outputNotChanged = (x) -> {
            List<Integer> ls = new ArrayList<>();
            ls.add(0);

            return new AddNum(x).to(ls).size() > ls.size();
        };

        CheckResult result = Property
                .def("Does not output on input")
                .forAll(Arbitrary.integer())
                .suchThat(outputNotChanged)
                .check();

        result.assertIsSatisfied();
    }
}