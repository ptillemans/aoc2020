import unittest
from collections import defaultdict
from typing import List


class Rule:
    def __init__(self, rule_str: str):
        self.rules = defaultdict(list)
        for rule in rule_str.splitlines():
            rule = rule.strip()
            rule = rule.split(": ")
            rule_number = rule[0]
            matches = rule[1].split(" | ")
            for match in matches:
                if '"' in match:
                    self.rules[rule_number].append(match.replace('"', ""))
                else:
                    self.rules[rule_number].append(match.split(" "))

    def match(self, message: str) -> bool:
        results = self.pr_match("0", 0, message)
        print(f"Results : {results}")
        return any([result == len(message) for result in results])

    def pr_match(self, rule: str, idx: int, message: str) -> List[int]:
        if idx == len(message):
            return []
        results = []
        for rules in self.rules[rule]:
            if type(rules) == type("a"):
                if rules == message[idx]:
                    # return the length matched
                    return [idx + 1]
                else:
                    return []
            start_idx = [idx]
            for sub_rule in rules:
                new_start_idx = []
                for i in start_idx:
                    sub_results = self.pr_match(sub_rule, i, message)
                    new_start_idx.extend(sub_results)
                start_idx = new_start_idx
            results.extend(start_idx)
        print(f"rule {rule} for {message}@{idx}: {results}")
        return results


def count_match_message(rule: Rule, messages: List[str]) -> int:
    return sum([rule.match(message) for message in messages])


class Testing(unittest.TestCase):
    def test_part_1(self):
        test_rules = '''0: 4 1 5
                1: 2 3 | 3 2
                2: 4 4 | 5 5
                3: 4 5 | 5 4
                4: "a"
                5: "b"'''

        rule = Rule(test_rules)
        self.assertTrue(rule.match("ababbb"))
        self.assertTrue(rule.match("abbbab"))
        self.assertFalse(rule.match("bababa"))
        self.assertFalse(rule.match("aaabbb"))
        self.assertFalse(rule.match("aaaabbb"))

    def test_simple(self):
        test_rules = '''0: 1 2
            1: 3 | 3 1
            2: 4 3 | 3 4
            3: "a"
            4: "b"'''

        rule = Rule(test_rules)
        self.assertTrue(rule.match("aba"))
        self.assertTrue(rule.match("aaba"))
        self.assertTrue(rule.match("aaaba"))
        self.assertTrue(rule.match("aaaaba"))
        self.assertTrue(rule.match("aaaaaba"))


def test_part_2():
    test_rules = """42: 9 14 | 10 1
        9: 14 27 | 1 26
        10: 23 14 | 28 1
        1: "a"
        11: 42 31
        5: 1 14 | 15 1
        19: 14 1 | 14 14
        12: 24 14 | 19 1
        16: 15 1 | 14 14
        31: 14 17 | 1 13
        6: 14 14 | 1 14
        2: 1 24 | 14 4
        0: 8 11
        13: 14 3 | 1 12
        15: 1 | 14
        17: 14 2 | 1 7
        23: 25 1 | 22 14
        28: 16 1
        4: 1 1
        20: 14 14 | 1 15
        3: 5 14 | 16 1
        27: 1 6 | 14 18
        14: "b"
        21: 14 1 | 1 14
        25: 1 1 | 1 14
        22: 14 14
        8: 42
        26: 14 22 | 1 20
        18: 15 15
        7: 14 5 | 1 21
        24: 14 1"""

    test_msgs = """abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
        bbabbbbaabaabba
        babbbbaabbbbbabbbbbbaabaaabaaa
        aaabbbbbbaaaabaababaabababbabaaabbababababaaa
        bbbbbbbaaaabbbbaaabbabaaa
        bbbababbbbaaaaaaaabbababaaababaabab
        ababaaaaaabaaab
        ababaaaaabbbaba
        baabbaaaabbaaaababbaababb
        abbbbabbbbaaaababbbbbbaaaababb
        aaaaabbaabaaaaababaa
        aaaabbaaaabbaaa
        aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
        babaaabbbaaabaababbaabababaaab
        aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"""

    messages = [msg.strip() for msg in test_msgs.splitlines()]
    rule = Rule(test_rules)

    # self.assertEqual(count_match_message(rule, messages), 3)

    test_rules = test_rules.replace("11: 42 31", "11: 42 31 | 42 11 31")
    test_rules = test_rules.replace("8: 42", "8: 42 | 42 8")
    rule = Rule(test_rules)
    # self.assertEqual(count_match_message(rule, messages), 12)
    # print(rule.match(messages[1]))
    print(rule.pr_match("11", 0, "bbaabaabba"))


def read_input(fname):
    with open(fname) as f:
        return f.read()


if __name__ == "__main__":
    data: List[str] = read_input("../input/day19.txt").split("\n\n")
    rule_str = data[0]
    messages = data[1].splitlines()
    rule = Rule(rule_str)
    # print("\tPart 1: {}".format(count_match_message(rule, messages)))

    updated_rules = rule_str.replace("11: 42 31", "11: 42 31 | 42 11 31")
    updated_rules = updated_rules.replace("8: 42", "8: 42 | 42 8")
    rule = Rule(updated_rules)
    # print("\tPart 2: {}".format(count_match_message(rule, messages)))

    # unittest.main()
    test_part_2()
    # print(rule.match(Testing.test_msgs[1]))
