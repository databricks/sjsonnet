load("constants.star", "get_list")

def test_global_mutation():
    g_list = get_list()
    print("Attempting to mutate global list...")
    g_list.append(4)
    print("ERROR: Should not reach here!")

test_global_mutation()
