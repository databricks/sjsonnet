load("constants.star", "get_list", "create_fresh_list")

def test_semantics():
    # 1. Test global list mutation (should fail)
    g_list = get_list()
    print("Global list:", g_list)
    try:
        g_list.append(4)
        print("ERROR: Successfully mutated global list!")
    except Error as e:
        print("SUCCESS: Caught expected error when mutating global list:", e)

    # 2. Test fresh list mutation (should succeed)
    f_list = create_fresh_list()
    print("Fresh list before:", f_list)
    f_list.append(7)
    print("Fresh list after:", f_list)
    if f_list == [4, 5, 6, 7]:
        print("SUCCESS: Successfully mutated fresh list.")
    else:
        print("ERROR: Fresh list mutation failed.")

test_semantics()
