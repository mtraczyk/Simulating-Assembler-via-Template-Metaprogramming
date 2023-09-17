#ifndef COMPUTER_H
#define COMPUTER_H

#include <cstdint>
#include <array>
#include <stdexcept>
#include <tuple>

using ull = uint64_t;

// Deklaracje wszystkich instrukcji.

struct Instruction {
};

template<typename... Instructions>
struct Program {
};

template<auto V>
struct Num {
    static_assert(std::is_integral_v < decltype(V) > , "Wrong type Num");
};

template<typename D, typename S>
struct Mov : Instruction {
};

template<typename D, typename S>
struct Add : Instruction {
};

template<typename D>
using Inc = Add<D, Num<1>>;

template<typename D>
using Dec = Add<D, Num<-1>>;

template<typename A>
struct Mem {
};

template<typename A, typename B>
struct Sub : Instruction {
};

template<ull W, typename N>
struct D : Instruction {
};

template<typename D, typename S>
struct And : Instruction {
};

template<typename D, typename S>
struct Or : Instruction {
};

template<typename D>
struct Not : Instruction {
};

template<typename D, typename S>
struct Cmp : Instruction {
};

template<ull w>
struct Lea {
};

template<ull w>
struct Jmp : Instruction {
};

template<ull w>
struct Jz : Instruction {
};

template<ull w>
struct Js : Instruction {
};

template<ull w>
struct Label : Instruction {
};

template<size_t MemorySize, typename T>
class Computer {
    using memory_var = std::array<ull, MemorySize>;
    using memory_t = std::array<T, MemorySize>;
    using uns_proc_t = typename std::make_unsigned<T>::type;
    using proc_t = T;

    constexpr static ull get_idx(memory_var &tab, ull val, size_t size, size_t i = 0) {
        i < size ? true : throw std::out_of_range("Wrong id!");

        return tab[i] == val ? i : get_idx(tab, val, size, i + 1);
    }

    // Struktura, w której następuje wywołanie ewaluatora instrukcji.
    template<typename S, typename P>
    struct ASMProgram;

    template<typename S, typename... Instructions>
    struct ASMProgram<S, Program<Instructions...>> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            Evaluator<S, Instructions...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja adresów.
    template<typename A>
    struct AddrEvaluator {
    };

    template<auto V>
    struct AddrEvaluator<Mem<Num<V>>> {
        constexpr static uns_proc_t value = uns_proc_t(V);

        constexpr static uns_proc_t find_addr(memory_t &, memory_var &) {
            return value;
        }
    };

    template<ull Arg1>
    struct AddrEvaluator<Lea<Arg1>> {
        constexpr static uns_proc_t find_addr(memory_t &, memory_var &mem_v) {
            return get_idx(mem_v, Arg1, MemorySize);
        }
    };

    template<ull Arg1>
    struct AddrEvaluator<Mem<Lea<Arg1>>> {
        constexpr static uns_proc_t find_addr(memory_t &, memory_var &mem_v) {
            return get_idx(mem_v, Arg1, MemorySize);
        }
    };

    template<typename N>
    struct AddrEvaluator<Mem<N>> {
        constexpr static uns_proc_t find_addr(memory_t &mem, memory_var &mem_v) {
            return mem[AddrEvaluator<N>::find_addr(mem, mem_v)];
        }
    };

    // Ewaluacja instrukcji.
    template<typename S, typename... Instructions>
    struct Evaluator {
        constexpr static void evaluate(memory_t &, memory_var &, int &, int &) {

        }
    };

    template<typename S, typename I, typename... Rest>
    struct Evaluator<S, I, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Mov.
    template<typename S, typename Arg1, typename Arg2, typename... Rest>
    struct Evaluator<S, Mov<Arg1, Mem<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = mem[AddrEvaluator<Arg2>::find_addr(mem, mem_v)];
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    template<typename S, typename Arg1, auto Arg2, typename... Rest>
    struct Evaluator<S, Mov<Arg1, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = proc_t(Arg2);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Add.
    template<typename S, typename Arg1, typename Arg2, typename... Rest>
    struct Evaluator<S, Add<Arg1, Mem<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = mem[addr] + mem[AddrEvaluator<Arg2>::find_addr(mem, mem_v)];
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    template<typename S, typename Arg1, auto Arg2, typename... Rest>
    struct Evaluator<S, Add<Arg1, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] += proc_t(Arg2);
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji And.
    template<typename S, typename Arg1, typename Arg2, typename... Rest>
    struct Evaluator<S, And<Arg1, Mem<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = mem[addr] & mem[AddrEvaluator<Arg2>::find_addr(mem, mem_v)];
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    template<typename S, typename Arg1, auto Arg2, typename... Rest>
    struct Evaluator<S, And<Arg1, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = mem[addr] & proc_t(Arg2);
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Or.
    template<typename S, typename Arg1, auto Arg2, typename... Rest>
    struct Evaluator<S, Or<Arg1, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = mem[addr] | proc_t(Arg2);
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    template<typename S, typename Arg1, typename Arg2, typename... Rest>
    struct Evaluator<S, Or<Arg1, Mem<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = mem[addr] | mem[AddrEvaluator<Arg2>::find_addr(mem, mem_v)];
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Not.
    template<typename S, typename Arg1, typename... Rest>
    struct Evaluator<S, Not<Arg1>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] = ~mem[addr];
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Sub.
    template<typename S, typename Arg1, typename Arg2, typename... Rest>
    struct Evaluator<S, Sub<Arg1, Mem<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] -= mem[AddrEvaluator<Arg2>::find_addr(mem, mem_v)];
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    template<typename S, typename Arg1, auto Arg2, typename... Rest>
    struct Evaluator<S, Sub<Arg1, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            mem[addr] -= proc_t(Arg2);
            set_flags(ZF, SF, mem[addr]);
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Cmp.
    template<typename S, typename Arg1, typename Arg2, typename... Rest>
    struct Evaluator<S, Cmp<Arg1, Mem<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            set_flags(ZF, SF, proc_t(mem[addr] - mem[AddrEvaluator<Arg2>::find_addr(mem, mem_v)]));
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    template<typename S, typename Arg1, auto Arg2, typename... Rest>
    struct Evaluator<S, Cmp<Arg1, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            uns_proc_t addr = AddrEvaluator<Arg1>::find_addr(mem, mem_v);

            addr < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            set_flags(ZF, SF, proc_t(mem[addr] - Arg2));
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    template<typename S, auto Arg1, auto Arg2, typename... Rest>
    struct Evaluator<S, Cmp<Num<Arg1>, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            set_flags(ZF, SF, proc_t(Arg1 - Arg2));
            Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Jmp.
    template<typename S, ull Arg, typename... Rest>
    struct Evaluator<S, Jmp<Arg>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            ASMProgramJump<S, Arg, S>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Jz.
    template<typename S, ull Arg, typename... Rest>
    struct Evaluator<S, Jz<Arg>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            ZF == 1 ? ASMProgramJump<S, Arg, S>::evaluate(mem, mem_v, ZF, SF)
                    : Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Ewaluacja instrukcji Js.
    template<typename S, ull Arg, typename... Rest>
    struct Evaluator<S, Js<Arg>, Rest...> {
        constexpr static void evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            SF == 1 ? ASMProgramJump<S, Arg, S>::evaluate(mem, mem_v, ZF, SF)
                    : Evaluator<S, Rest...>::evaluate(mem, mem_v, ZF, SF);
        }
    };

    // Struktura pomocnicza, potrzebna do oddzielenia Instructions...
    // od Program<Instructions...>. Wywołuje ewaluator służący do znalezienia odpowiedniego Label.
    template<typename S, ull Arg, typename P>
    struct ASMProgramJump;

    template<typename S, ull Arg, typename... Instructions>
    struct ASMProgramJump<S, Arg, Program<Instructions...>> {
        constexpr static void
        evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF) {
            bool label_was_found = false;
            Evaluator_to_find_label<S, Arg, Instructions...>::evaluate(mem, mem_v, ZF, SF,
                                                                       label_was_found);
            !label_was_found ? throw std::invalid_argument("Error: label does not exist!") : true;
        }
    };

    // Ewaluator służący do znalezienia odpowiedniej instrukcji Label.
    template<typename S, ull Arg, typename... Instructions>
    struct Evaluator_to_find_label {
        constexpr static void evaluate(memory_t &, memory_var &, int &, int &, bool &) {

        }
    };

    template<typename S, ull Arg, typename I, typename... Rest>
    struct Evaluator_to_find_label<S, Arg, I, Rest...> {
        constexpr static void
        evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF, bool &label_was_found) {
            Evaluator_to_find_label<S, Arg, Rest...>::evaluate(mem, mem_v, ZF, SF, label_was_found);
        }
    };

    template<typename S, ull Arg1, ull Arg2, typename... Rest>
    struct Evaluator_to_find_label<S, Arg1, Label<Arg2>, Rest...> {
        constexpr static void
        evaluate(memory_t &mem, memory_var &mem_v, int &ZF, int &SF, bool &label_was_found) {
            Arg1 == Arg2 ? label_was_found = true : true;
            Arg1 == Arg2 ? ASMProgram<S, Program<Rest...>>::evaluate(mem, mem_v, ZF, SF) :
            Evaluator_to_find_label<S, Arg1, Rest...>::evaluate(mem, mem_v, ZF, SF,
                                                                label_was_found);
        }
    };

    // Funkcja, która służy do ustawienia flag procesora.
    constexpr static void set_flags(int &ZF, int &SF, T val) {
        ZF = (val == 0 ? 1 : 0);
        SF = (val < 0 ? 1 : 0);
    }

    template<typename P>
    struct ASMProgramVar;

    // Program odpowiedzialny za przetworzenie deklaracji.
    template<typename... Instructions>
    struct ASMProgramVar<Program<Instructions...>> {
        constexpr static void evaluate(memory_var &mem, memory_t &mem_v) {
            static_assert(
                    ((std::is_base_of
                            <Instruction, Instructions>::value)&& ... && true),
                    "Error: a program should contain instructions only!");
            DEvaluator<Instructions...>::evaluate(mem, 0, mem_v);
        }
    };

    template<typename... Instructions>
    struct DEvaluator {
        constexpr static void evaluate(memory_var &, size_t, memory_t &) {
        }
    };

    // Devaluator pomijący wszystkie nie interesujące kwestie z perspektywy
    // deklarowania zmiennych.
    template<typename I, typename... Rest>
    struct DEvaluator<I, Rest...> {
        constexpr static void evaluate(memory_var &mem, size_t idx, memory_t &mem_v) {
            DEvaluator<Rest...>::evaluate(mem, idx, mem_v);
        }
    };

    // Deklarowanie nowej zmiennej.
    template<ull Arg1, auto Arg2, typename... Rest>
    struct DEvaluator<D<Arg1, Num<Arg2>>, Rest...> {
        constexpr static void evaluate(memory_var &mem, size_t idx, memory_t &mem_v) {
            // Pobieranie wartości.
            ull var1 = Arg1;
            uns_proc_t value = uns_proc_t(Arg2);

            idx < MemorySize ? true : throw std::out_of_range("Error: an incorrect address!");

            // Zapamiętujemy identyfikatory.
            mem[idx] = var1;
            mem_v[idx] = value;
            DEvaluator<Rest...>::evaluate(mem, idx + 1, mem_v);
        }
    };

    // Devaluator wykrywający niepoprawne wywołania.
    template<ull Arg1, typename Arg2, typename... Rest>
    struct DEvaluator<D<Arg1, Arg2>, Rest...> {
        constexpr static void evaluate(memory_var &, size_t, memory_t &) {
            true ? throw std::domain_error("Incorrect D declaration!") : true;
        }
    };

public:

    template<typename P>
    constexpr static auto boot() {
        std::tuple<memory_t, memory_var, int, int> info = std::make_tuple(memory_t{0},
                                                                          memory_var{0}, -1, -1);
        ASMProgramVar<P>::evaluate(std::get<1>(info), std::get<0>(info));
        ASMProgram<P, P>::evaluate(std::get<0>(info), std::get<1>(info), std::get<2>(info),
                                   std::get<3>(info));
        return std::get<0>(info);
    }
};

// Klasa odpowiedzialna za hashowanie napisu char * na uint64_t.
class hash_c {
    // Stała przechowywująca ilość cyfr.
    constexpr static int numOfDigits = 10;
    // Stała przechowywująca numer kodu pierwszej cyfry, w ASCII.
    constexpr static int ascii_code_of_first_number = 47;
    // Stała indeksująca kod ascii pierwszej litery pomniejszony o
    // ilość cyfr.
    constexpr static int index_of_first_letter = 54;
    // Stała przechowywująca odległość w kodzie ASCII pomiędzy małą i dużą literą.
    constexpr static int distance_beetwen_letterrs = 32;
    // Zwraca ilość cyfr i małych liter.
    constexpr static int number_of_correct_chars = 36;

    // Zamienia litere na dużą.
    static constexpr char toUpper(char w) {
        return ('a' <= w && w <= 'z') ? char((int) w - distance_beetwen_letterrs) : w;
    }

    // Sprawdza poprawność znaku.
    static constexpr bool correctLetter(char w) {
        return ('0' <= w && w <= '9') || ('A' <= w && w <= 'Z') || ('a' <= w && w <= 'z');
    }

    // Zwraca kod przyporządkowany do danego znaku.
    static constexpr int code(char w) {
        return (w <= '9') ? int(w) - ascii_code_of_first_number : int(w) - index_of_first_letter;
    }

    // Zwraca wielkość ilość znaków napisu w.
    static constexpr size_t array_size(const char *w, size_t N = 0) {
        return w[N] == '\0' ? N : array_size(w, N + 1);
    }

    // Sprawdza czy całe słowo w jest poprawne składniowo.
    static constexpr bool correctWord(const char *w, size_t N = 0) {
        return w[N] == '\0' ? true :
               (correctLetter(w[N]) ? correctWord(w, N + 1) :
                throw std::invalid_argument("Incorrect letter"));
    }

    // Hashuje słowo na ull.
    static constexpr ull hash_word(const char *w, size_t N, size_t idx = 0, ull M = 1) {
        return (idx >= N) ? 0 : code(toUpper(w[idx])) * M +
                                hash_word(w, N, idx + 1, M * number_of_correct_chars);
    }

public :
    // Zaprzyjaźniona funkcja Id z głównego interfejsu, która zwraca zakodowane słowo.
    friend constexpr ull Id(const char *w);
};

constexpr ull Id(const char *w) {
    // Linia sprawdzająca czy długość danego słowo jest prawidłowa (pomiędzy 1, a 6) oraz
    // czy wszystkie litery są poprawne.
    (hash_c::array_size(w) <= 6 && hash_c::array_size(w) >= 1 && hash_c::correctWord(w)) ?
    true : throw std::invalid_argument("Wrong Id");

    // Zwracamy zahashowane słowo.
    return hash_c::hash_word(w, hash_c::array_size(w));
}

#endif // COMPUTER_H