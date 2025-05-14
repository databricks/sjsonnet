local helper = {
  local this = self,

  bar(param, unusedParam)::
    param,

  foo(param)::
    local sum_from_one_to_n(counter, n) =
      if n == 0 then
        counter
      else
        sum_from_one_to_n(this.bar(counter + n, error 'Kaboom!'), n - 1) tailstrict;

    local result = sum_from_one_to_n(0, param);
    result,
};

{
  bar: helper.foo(10),
}
