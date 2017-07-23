#undef SLOT

#include <ecl/ecl.h>
#include <eql5/eql.h>
#include <QApplication>
#include <QTextCodec>
#include <QSettings>
#include <QTranslator>

extern "C" void init_lib_NEXT__ALL_SYSTEMS(cl_object);

int catch_all_qexec() {
    int ret = 0;
    CL_CATCH_ALL_BEGIN(ecl_process_env()) {
        ret = QApplication::exec(); }
    CL_CATCH_ALL_END;
    return ret; }

int main(int argc, char** argv) {

    EQL::ini(argv);

    QCoreApplication::setAttribute(Qt::AA_ShareOpenGLContexts); // for Qt WebEngine
    QApplication qapp(argc, argv);

    QTextCodec* utf8 = QTextCodec::codecForName("UTF-8");
    QTextCodec::setCodecForLocale(utf8);

    EQL eql;

#ifdef Q_OS_WIN
    // print output would crash program
    eql.ignoreIOStreams();
#endif

    eql.exec(init_lib_NEXT__ALL_SYSTEMS);

    return catch_all_qexec(); } // closing the main/last window will quit the program
