// skip_list.cpp — C++20 reference implementation of skip_list.a4.
//   g++ -std=c++20 -O2 -o skip_list skip_list.cpp && ./skip_list

#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <random>
#include <unordered_map>
#include <utility>
#include <vector>

constexpr int MAX_LEVEL = 4;

static std::mt19937 rng{42};

static int random_level() {
    int lvl = 0;
    while (lvl < MAX_LEVEL - 1 && (rng() % 2) == 1) ++lvl;
    return lvl;
}

static void sorted_insert(std::vector<int>& lst, int x) {
    auto it = std::lower_bound(lst.begin(), lst.end(), x);
    lst.insert(it, x);
}

class SkipList {
public:
    std::unordered_map<int, int> data;
    std::array<std::vector<int>, MAX_LEVEL> levels;

    void insert(int key, int value) {
        if (!data.contains(key)) {
            int lvl = random_level();
            for (int i = 0; i <= lvl; ++i) sorted_insert(levels[i], key);
        }
        data[key] = value;
    }

    void remove(int key) {
        if (!data.contains(key)) return;
        for (auto& lvl : levels) {
            auto it = std::find(lvl.begin(), lvl.end(), key);
            if (it != lvl.end()) lvl.erase(it);
        }
        data.erase(key);
    }

    bool has(int key) const   { return data.contains(key); }
    int  get(int key) const   { return data.at(key); }
    std::size_t size() const { return data.size(); }
    const std::vector<int>& keys() const { return levels[0]; }
};

int main() {
    // sorted-insert
    {
        std::vector<int> lst;
        for (int x : {3, 1, 5, 2}) sorted_insert(lst, x);
        assert(lst.front() == 1);
        assert(lst.size() == 4);
        std::cout << "sorted-insert: ok\n";
    }

    // empty
    {
        SkipList sl;
        assert(sl.size() == 0);
        std::cout << "sl-new: ok\n";
    }

    // single
    {
        SkipList sl;
        sl.insert(10, 100);
        assert(sl.has(10));
        assert(sl.get(10) == 100);
        assert(sl.size() == 1);
        std::cout << "single insert: ok\n";
    }

    // multi
    {
        SkipList sl;
        for (auto [k, v] : {std::pair{5,50}, {3,30}, {8,80}, {1,10}})
            sl.insert(k, v);
        assert(sl.size() == 4);
        assert(sl.get(1) == 10);
        assert(sl.get(3) == 30);
        assert(sl.get(5) == 50);
        assert(sl.get(8) == 80);
        assert(sl.keys().front() == 1);
        std::cout << "multi insert: ok\n";

        sl.remove(3);
        assert(sl.size() == 3);
        assert(!sl.has(3));
        std::cout << "delete: ok\n";

        sl.remove(99);
        assert(sl.size() == 3);

        sl.insert(5, 999);
        assert(sl.get(5) == 999);
        std::cout << "update: ok\n";
    }

    // larger
    {
        SkipList sl;
        for (auto [k, v] : {std::pair{20,200}, {10,100}, {30,300},
                            {15,150}, {25,250}, {5,50}, {35,350}})
            sl.insert(k, v);
        assert(sl.size() == 7);
        assert(sl.get(5) == 50);
        assert(sl.get(35) == 350);
        std::cout << "Skip list with 7 entries:\n";
        for (int i = 0; i < MAX_LEVEL; ++i)
            std::cout << "  Level " << i << ": " << sl.levels[i].size() << " keys\n";
        std::cout << "large dataset: ok\n";
    }

    return 0;
}
