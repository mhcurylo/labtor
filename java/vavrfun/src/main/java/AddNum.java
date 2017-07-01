import java.util.List;

public class AddNum {
    Integer num;
    AddNum(Integer n) { num = n; }
    public List<Integer> to(List<Integer> ls) {
        ls.add(0, num);
        return ls;
    }
    public void setNum(Integer n) {num = n;}
}
