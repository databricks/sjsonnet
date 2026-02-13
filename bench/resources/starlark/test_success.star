load("constants.star", "get_list", "create_fresh_list")

def test_fresh_mutation():
    f_list = create_fresh_list()
    print("Fresh list before:", f_list)
    f_list.append(7)
    print("Fresh list after:", f_list)
    if f_list == [4, 5, 6, 7]:
        print("SUCCESS: Fresh list mutation worked as expected.")

test_fresh_mutation()
