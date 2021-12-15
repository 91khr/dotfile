#if 0
dotpath=$(readlink -f $(dirname $0))
cd $dotpath
flags='-std=c++20 -o .install -DDOTPATH="\"'$dotpath'\"" -DHOMEPATH="\"'$HOME'\""'
CXXFLAGS="$flags" make -sf <(echo -e '.install:install.cpp; $(CXX) $< $(CXXFLAGS)') .install || exit $?
exec ./.install $@
#endif
// {{{ Premable
#include <cstdio>
#include <fstream>
#include <cstdlib>
#include <string>
#include <filesystem>
#include <concepts>
#include <list>
#include <tuple>
#include <algorithm>
#include <numeric>
#include <unordered_map>
#include <unordered_set>
#include <functional>
using std::string;
using std::to_string;
using std::move;
namespace fs = std::filesystem;
// }}} End premable



// {{{ Environment & options
struct {
    string dotpath {}, homepath {};
    const bool iswin =
#ifdef _WIN32
        true
#else
        false
#endif
        ;  // Resume the Vim indent ><
    const char *helpmsg = R"(Usage: %s <options> <configs>
A dotfile installation helper. If no option is specified, default to -i,
if no config is specified either, default is -l.
A special config 'available' stands for all config available on the system;
and 'everything' stands for all config

  -h, --help    print the message and quit
  -i, --install install given configurations
  -l, --list    list information of given configurations, grouped by their state.
                if not given, list all configurations
  -r, --reached list installed configurations
  -u, --pending list configurations not yet installed
)";
} env;
// }}} End environment & options



// {{{1 Config helpers
template<class T>
concept ConfigInstaller = requires(const T a) {
    { a.is_installed() } -> std::convertible_to<bool>;
} && requires(T a) {
    a.install();
} && std::move_constructible<T> && std::destructible<T>;

struct ConfigInfo
{
public:
    inline static const char *OSName[] = { "UNIX", "Win", "All" };
    enum OSType {
        UNIX, Win, AllOS,
    };

    const char *name;
    OSType os;
    const char *description;

private:
    struct Data
    {
        std::function<void(void*)> constructor;
        void (*install)(void*) = [] (auto) {};
        bool (*is_installed)(void*) = [] (auto) { return false; };
        void (*destructor)(void*) = [] (auto) {};
    };
    char *data;

public:
    // {{{ Constructors and destructors
    template<class T>
    ConfigInfo(const char *n, OSType o, const char *desc, T inst)
        requires ConfigInstaller<decltype(inst())>
        : name(n), os(o), description(desc),
          data(new char[sizeof(Data) + sizeof(decltype(inst()))])
    {
        using InstallerT = decltype(inst());
        new(data) Data {
            [inst=move(inst)] (void *data) { new(data) InstallerT(inst()); },
            [] (void *data) { static_cast<InstallerT*>(data)->install(); },
            [] (void *data) { return static_cast<InstallerT*>(data)->is_installed(); },
            [] (void *data) { static_cast<InstallerT*>(data)->~InstallerT(); },
        };
    }
    ConfigInfo(const ConfigInfo &) = delete;
    ConfigInfo &operator=(const ConfigInfo &) = delete;
    ConfigInfo(ConfigInfo &&o)
        : name(o.name), os(o.os), description(o.description), data(o.data)
    { o.data = nullptr; }
    ConfigInfo &operator=(ConfigInfo &&o)
    {
        this->~ConfigInfo();
        new(this) ConfigInfo(move(o));
        return *this;
    }
    ~ConfigInfo()
    {
        if (data)
        {
            reinterpret_cast<Data*>(data)->destructor(data + sizeof(Data));
            delete data;
        }
    }
    // }}} End constructors and destructors

    void init() { reinterpret_cast<Data*>(data)->constructor(data + sizeof(Data)); }
    void install() { reinterpret_cast<Data*>(data)->install(data + sizeof(Data)); }
    bool is_installed() { return reinterpret_cast<Data*>(data)->is_installed(data + sizeof(Data)); }
};

template<ConfigInstaller T, ConfigInstaller U>
auto operator&(T &&a, U &&b)
{
    struct {
        T a; U b;
        void install() { a.install(); b.install(); }
        bool is_installed() const { return a.is_installed() && b.is_installed(); }
    } res{a, b};
    return res;
}

std::unordered_map<string, ConfigInfo> conf_list;
#define add_conf_impl(ln) struct AddConf##ln { \
    AddConf##ln(ConfigInfo &&info) { conf_list.insert(std::pair { info.name, move(info) }); } \
} addconf_var##ln = ConfigInfo
#define add_conf_2(ln) add_conf_impl(ln)
#define add_conf add_conf_2(__LINE__)

// {{{ BindFnConfig
class BindFnConfig
{
public:
    std::function<void()> install;
    std::function<bool()> is_installed;
};
// }}} End BindFnConfig

// {{{ InvokerConfig
class InvokerConfig
{
private:
    fs::path file;
    const string begline, endline;
    string commands;
    std::list<string> readfile() const
    {
        std::list<string> res;
        std::ifstream in(file);
        if (!in)
            return res;
        while (!in.eof())
        {
            res.push_back("");
            std::getline(in, res.back());
        }
        return res;
    }
public:
    InvokerConfig(fs::path f, string comment_prefix, string name, string cmd)
        : file(env.homepath / f),
          begline(comment_prefix + " {{{ Generated by dotfile installer: " + name),
          endline(comment_prefix + " }}} End dotfile installer generated: " + name),
          commands(cmd) { if (commands.back() != '\n') commands.push_back('\n'); }
    void install()
    {
        auto ctnt = readfile();
        auto insertion = begline + "\n" + commands + endline;
        auto beg = std::find(ctnt.begin(), ctnt.end(), begline);
        auto end = std::find(beg, ctnt.end(), endline);
        if (end == ctnt.end())
            ctnt.push_back(insertion);
        else
        {
            ctnt.insert(beg, insertion);
            ctnt.erase(beg, ++end);
        }
        if (!fs::exists(file.parent_path()))
            fs::create_directories(file.parent_path());
        std::ofstream out(file);
        // When there's eol at the end of file, do not include the empty line
        if (ctnt.back().empty())
            ctnt.pop_back();
        for (auto line : ctnt)
            out << line << "\n";
        out.close();
    }
    bool is_installed() const
    {
        auto ctnt = readfile();
        auto beg = std::find(ctnt.begin(), ctnt.end(), begline);
        auto end = std::find(beg, ctnt.end(), endline);
        if (end == ctnt.end())
            return false;
        else
            return std::accumulate(++beg, end, string(), [] (auto a, auto b) {
                return a + b + "\n";
            }) == commands;
    }
};
// }}} End InvokerConfig

// {{{ SymlinkConfig
class SymlinkConfig
{
private:
    fs::path src, dst;
public:
    SymlinkConfig(fs::path s, fs::path d)
        : src(env.dotpath / s), dst(env.homepath / d) {}
    void install()
    {
        if (fs::is_directory(src))
        {
            for (fs::recursive_directory_iterator it { src, fs::directory_options::follow_directory_symlink };
                    it != decltype(it)(); ++it)
            {
                auto dstpath = dst / it->path().lexically_relative(src);
                if (it->is_directory())
                    fs::create_directories(dstpath);
                else if (!fs::exists(dstpath))
                    fs::create_symlink(*it, dstpath);
                else
                    fprintf(stderr, "%s already exists\n", dstpath.c_str());
            }
        }
        else
            fs::create_symlink(src, dst);
    }
    bool is_installed() const
    {
        if (fs::is_directory(src))
        {
            for (fs::recursive_directory_iterator it { src, fs::directory_options::follow_directory_symlink };
                    it != decltype(it)(); ++it)
                if (!fs::exists(dst / it->path().lexically_relative(src)))
                    return false;
            return true;
        }
        else
            return fs::exists(dst);
    }
};
// }}} End SymlinkConfig

string to_string(string s) { return s; }
// {{{ fn format
string format(string pat, auto ...args)
{
    // Stringfy the args
    std::vector<string> argstr;
    argstr.reserve(sizeof...(args));
    auto putargs = [&argstr] (auto f, auto car, auto ...cdr) {
        argstr.push_back(to_string(car));
        if constexpr (sizeof...(cdr))
            return f(f, cdr...);
    };
    if constexpr (sizeof...(args))
        putargs(putargs, args...);
    // Iterate and replace format patterns
    auto argit = argstr.begin();
    for (auto pos = pat.find('%'); pos != pat.npos; pos = pat.find('%', pos))
        if (++pos < pat.size())
            switch (pat[pos])
            {
            case 'D':
                pat.replace(pos - 1, 2, env.dotpath);
                break;
            case 'H':
                pat.replace(pos - 1, 2, env.homepath);
                break;
            case 'o':
                if (argit == argstr.end())
                {
                    fprintf(stderr, "Too few arguments for format\n");
                    exit(3);
                }
                pat.replace(pos - 1, 2, *argit++);
                break;
            case '%':
                pat.replace(pos, 1, "");
                break;
            default:
                fprintf(stderr, "Unrecognized pattern %c\n", pat[pos]);
                exit(3);
            }
        else
        {
            fprintf(stderr, "Trailing %% in pattern string\n");
            exit(3);
        }
    if (argit != argstr.end())
    {
        fprintf(stderr, "Too much arguments for format\n");
        exit(3);
    }
    return pat;
}
// }}} End fn format
// }}}1 End config helpers



// {{{ Main
int main(int, char **argv)
{
    // {{{ Process default parameters
    env.dotpath =
#ifndef DOTPATH
        fs::absolute(fs::path(argv[0]).parent_path());
#else
        DOTPATH;
#endif  // DOTPATH
        ;  // Resume the Vim indent ><
    env.homepath =
#ifndef HOMEPATH
        std::getenv(env.iswin ? "USERPROFILE" : "HOME")
#else
        HOMEPATH
#endif  // HOMEPATH
        ;  // Resume the Vim indent ><
    // }}} End process default parameters

    fs::current_path(fs::path(argv[0]).remove_filename());

    // {{{ Process command-line arguments
    enum {
        Unset, Install, List, Reached, Pending,
    } operation(Unset);
    bool force = false;
    std::unordered_map<string, std::function<void()>> longopt_fns;
    std::unordered_map<char, std::function<void()>> shortopt_fns;
    std::unordered_set<string> selected_config;
    auto check_operation = [&] {
        if (operation != Unset)
        {
            fprintf(stderr, "Duplicate actions!\n");
            exit(1);
        }
    };
    auto copyall = [&] (bool everything) {
        selected_config.clear();
        for (auto &it : conf_list)
            if (everything || it.second.os == ConfigInfo::AllOS ||
                    ((it.second.os == ConfigInfo::Win) == env.iswin))
                selected_config.insert(it.first);
    };
    for (auto it : std::initializer_list<std::tuple<const char*, char, std::function<void()>>> {
        { "help", 'h', [&] { printf(env.helpmsg, argv[0]); exit(0); }, },
        { "install", 'i', [&] { check_operation(); operation = Install; }, },
        { "list", 'l', [&] { check_operation(); operation = List; }, },
        { "reached", 'r', [&] { check_operation(); operation = Reached; copyall(false); }, },
        { "pending", 'u', [&] { check_operation(); operation = Pending; copyall(false); }, },
        { "force", 'f', [&] { force = true; }, },
    })
    {
        longopt_fns[std::get<0>(it)] = std::get<2>(it);
        shortopt_fns[std::get<1>(it)] = std::get<2>(it);
    }
    for (char **arg = argv + 1; *arg; ++arg)
    {
        string curarg = *arg;
        if (curarg.starts_with("--"))  // Long option
        {
            bool ok = false;
            for (auto it : longopt_fns)
                if (it.first.starts_with(curarg.substr(2)))
                {
                    it.second();
                    ok = true;
                    break;
                }
            if (!ok)
                fprintf(stderr, "Error: unrecognized option %s\n", *argv);
        }
        else if (curarg.starts_with("-"))  // Short arg
        {
            for (auto ch : curarg.substr(1))
                if (auto it = shortopt_fns.find(ch); it != shortopt_fns.end())
                    it->second();
                else
                    fprintf(stderr, "Error: unrecognized option %c\n", ch);
        }
        else  // Ordinary config
        {
            if (curarg == "everything" || curarg == "available")
                copyall(curarg == "everything");
            else if (!selected_config.contains(curarg))
            {
                if (conf_list.find(curarg) != conf_list.end())
                    selected_config.insert(curarg);
                else
                    fprintf(stderr, "Unrecognized config: %s\n", curarg.c_str());
            }
        }
    }
    // }}} End processing command-line arguments
    for (auto &conf : conf_list)
        conf.second.init();

    // Perform operation
    if (operation == Unset)
        operation = selected_config.empty() ? List : Install;
    if (operation == List && selected_config.empty())
        copyall(true);
    std::list<ConfigInfo> installed, pending;
    for (auto &name : selected_config)
    {
        auto &it = conf_list.find(name)->second;
        (it.is_installed() ? installed : pending).push_back(move(it));
    }
    switch (operation)
    {
    case Unset:  // Impossible ><
    case List:
        puts("Installed:");
        for (auto &it : installed)
            printf("  %s(%s): %s\n", it.name, it.OSName[it.os], it.description);
        puts("Not yet installed:");
        for (auto &it : pending)
            printf("  %s(%s): %s\n", it.name, it.OSName[it.os], it.description);
        break;
    case Reached:
    case Pending:
        for (auto &it : operation == Reached ? installed : pending)
            printf("%s ", it.name);
        puts("");
        break;
    case Install:
        if (force)
            std::move(installed.begin(), installed.end(), std::back_inserter(pending));
        for (auto &it : pending)
        {
            printf("Installing %s\n", it.name);
            it.install();
        }
        break;
    }
}
// }}} End main



add_conf { "awesome", ConfigInfo::UNIX, "AwesomeWM config",
    [] { return InvokerConfig { ".config/awesome/rc.lua", "--", "awesome",
        format(R"(loadfile("%D/awesome/rc.lua")("%D"))") }; },
};

add_conf { "emacs", ConfigInfo::AllOS, "Emacs config",
    [] { return InvokerConfig { ".emacs.d/init.el", ";;", "emacs conf",
        format("(load-file \"%D/emacs/init.el\")") }; },
};

add_conf { "firefox", ConfigInfo::AllOS, "Firefox css & chrome",
    [] { return SymlinkConfig { "firefox", ".mozilla/firefox/userprofile" }; },
};

add_conf { "mutt", ConfigInfo::UNIX, "Mutt config",
    [] { return InvokerConfig { ".mutt/muttrc", "#", "mutt",
        format("set my_muttrc_path = %D/mutt\nsource %D/mutt/muttrc") }; },
};

add_conf { "profile", ConfigInfo::UNIX, "Default user profiles",
    [] {
        return InvokerConfig { ".profile", "#", "profile", format("source %D/profile/profile") } &
            InvokerConfig { ".xprofile", "#", "profile", format("source %D/profile/xprofile") } &
            InvokerConfig { ".Xresources", "!", "profile", format(R"(#include "%D/profile/Xresources"
#include "%D/profile/Solarizedxterm/.Xdefaults")") };
    },
};

add_conf { "utils", ConfigInfo::AllOS, "Some utilities to make life better",
    [] { return SymlinkConfig { "utils", "bin" }; },
};

add_conf { "vim", ConfigInfo::AllOS, "Vim config",
    [] {
        return InvokerConfig { ".vimrc", "\"", "vimrc", format("source %D/vim/vimrc") } &
            SymlinkConfig { "vim/vimfiles", ".vim" };
    },
};

add_conf { "zsh", ConfigInfo::AllOS, "Z shell config",
    [] { return InvokerConfig { ".zshrc", "#", "zshrc", format("source %D/zsh/zshrc") }; },
};

add_conf { "system", ConfigInfo::AllOS, "System setup",
    [] {
        return BindFnConfig {
            .install = [] {
                system(format("%o %D/system/%o%o",
                            env.iswin ? "powershell" : "sh",
                            env.iswin ? "win.ps1" : "arch.sh",
                            "").c_str());
            },
            .is_installed = [] { return true; },
        };
    },
};



// vim: fdm=marker
