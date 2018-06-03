package com.typelead.gradle.eta.plugins;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.nio.file.Paths;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;

import groovy.lang.Closure;

import org.gradle.api.Action;
import org.gradle.api.DomainObjectCollection;
import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.ProjectDependency;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;
import org.gradle.api.logging.LogLevel;
import org.gradle.api.plugins.BasePlugin;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.plugins.JavaPluginConvention;
import org.gradle.api.tasks.SourceSet;
import org.gradle.api.tasks.testing.Test;
import org.gradle.api.tasks.testing.TestResult.ResultType;
import org.gradle.api.internal.tasks.testing.TestStartEvent;
import org.gradle.api.internal.tasks.testing.TestCompleteEvent;
import org.gradle.api.internal.tasks.testing.TestResultProcessor;
import org.gradle.api.internal.tasks.testing.JvmTestExecutionSpec;
import org.gradle.api.internal.tasks.testing.DefaultTestDescriptor;
import org.gradle.api.internal.tasks.testing.DefaultTestSuiteDescriptor;
import org.gradle.process.ExecResult;
import org.gradle.process.JavaExecSpec;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.utils.Version;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.api.NamingScheme;
import com.typelead.gradle.eta.api.ProguardFiles;
import com.typelead.gradle.eta.tasks.EtaInjectDependencies;
import com.typelead.gradle.eta.tasks.EtaInstallAllDependencies;
import com.typelead.gradle.eta.tasks.EtaRepl;
import com.typelead.gradle.eta.tasks.EtaResolveDependencies;
import com.typelead.gradle.eta.tasks.EtaSetupEnvironment;
import com.typelead.gradle.eta.internal.DefaultEtaConfiguration;
import com.typelead.gradle.eta.internal.DefaultEtaProjectDependency;
import com.typelead.gradle.eta.internal.EtlasMavenRepository;

/**
 * A {@link Plugin} which compiles and tests Eta sources.
 */
public class EtaBasePlugin implements Plugin<Project> {

    /* Constants */
    public static final String ETA_EXTENSION_NAME = "eta";
    public static final String TASK_GROUP_NAME = "EtaPlugin";

    public static final String ETA_INTERMEDIATES_DIRECTORY = "eta-intermediates";

    public static final String DEFAULT_ETA_MAIN_CLASS = "eta.main";

    public static final String ETA_CONFIGURATION_EXTENSION_NAME  = "eta";

    /* Tasks */
    public static final String ETA_SETUP_ENVIRONMENT_TASK_NAME = "setupEnvironmentEta";
    public static final String
        ETA_RESOLVE_DEPENDENCIES_TASK_NAME = "resolveDependenciesEta";

    public static final String
        ETA_INSTALL_ALL_DEPENDENCIES_TASK_NAME = "installAllDependenciesEta";

    public static final String ETA_REPL_TASK_NAME = "repl";

    private Project project;
    private EtaExtension extension;
    private EtlasMavenRepository mavenRepository;

    @Override
    public void apply(Project project) {
        this.project   = project;

        checkForValidGradleVersion();

        this.mavenRepository = new EtlasMavenRepository
            (project, new File(project.getGradle().getGradleUserHomeDir()
                               + File.separator + "caches" + File.separator
                               + "etlas"));

        project.getPlugins().apply(BasePlugin.class);

        EtaPluginConvention etaConvention = new EtaPluginConvention(project);
        project.getConvention().getPlugins().put("eta", etaConvention);

        createRootEtaExtension();

        addEtlasMavenRepository();

        addEtaExtensionForConfigurations();

        configureEtaRootTasks();

        configureInjectionTasks();

        createProguardFiles();

        configureTestTask();
    }

    private void checkForValidGradleVersion() {
        if (Version.create(project.getGradle().getGradleVersion())
            .isBefore(Version.create("4.3"))) {
            throw new GradleException("The Eta Gradle Plugin only supports Gradle 4.3+. You can temporarily disable this plugin and update your wrapper script with `./gradlew wrapper --gradle-version 4.7`.");
        }
    }

    private void createRootEtaExtension() {
        if (isRootProject()) {
            extension = project.getExtensions()
                .create(EtaBasePlugin.ETA_EXTENSION_NAME, EtaExtension.class, project);
        }
    }

    private void addEtlasMavenRepository() {
        if (isRootProject()) {
            project.allprojects(p -> {
                    p.getRepositories().maven
                        (repo -> {
                            repo.setName("EtlasMaven");
                            repo.setUrl(mavenRepository.getDirectory().toURI());
                        });
                });
        }
    }

    private void addEtaExtensionForConfigurations() {
        project.getConfigurations().all(this::populateEtaConfiguration);

    }

    private void populateEtaConfiguration(final Configuration configuration) {

        final DefaultEtaConfiguration etaConfiguration =
            ExtensionHelper.createExtension
            (configuration, ETA_CONFIGURATION_EXTENSION_NAME,
             DefaultEtaConfiguration.class, configuration, mavenRepository);

        DomainObjectCollection<EtaDependency> dependencies =
            etaConfiguration.getDependencies();

        configuration.getDependencies().all
            (dependency -> {
                if (dependency instanceof ProjectDependency) {
                    final ProjectDependency projectDependency =
                        (ProjectDependency) dependency;
                    dependencies.add(new DefaultEtaProjectDependency
                                     (projectDependency.getDependencyProject(),
                                      projectDependency.getTargetConfiguration()));
                } else if (dependency instanceof EtaDependency) {
                    dependencies.add((EtaDependency) dependency);
                }
            });
    }

    private void configureEtaRootTasks() {
        /* The global, consistent dependency resolution must be done in the
           root project. */
        if (isRootProject()) {
            EtaSetupEnvironment setupEnvironmentTask =
                project.getTasks().create(ETA_SETUP_ENVIRONMENT_TASK_NAME,
                                          EtaSetupEnvironment.class);

            EtaResolveDependencies resolveDependenciesTask =
                project.getTasks().create(ETA_RESOLVE_DEPENDENCIES_TASK_NAME,
                                          EtaResolveDependencies.class);

            EtaInstallAllDependencies installAllDependenciesTask =
                project.getTasks().create(ETA_INSTALL_ALL_DEPENDENCIES_TASK_NAME,
                                          EtaInstallAllDependencies.class);

            EtaRepl replTask =
                project.getTasks().create(ETA_REPL_TASK_NAME, EtaRepl.class);

            resolveDependenciesTask.setVersionsChanged
                (setupEnvironmentTask.getVersionsChanged());

            resolveDependenciesTask.dependsOn(setupEnvironmentTask);

            installAllDependenciesTask.dependsOn(resolveDependenciesTask);

            replTask.setDestinationDir
                (project.provider(() -> project.getLayout().getProjectDirectory()));
            replTask.dependsOn(setupEnvironmentTask);

            // We need to wait until the Eta dependencies of *all* subprojects
            // have been configured.
            project.getGradle().projectsEvaluated(gradle -> {
                    if (extension.shouldPreInstallDependencies()) {
                        setupEnvironmentTask.setupEnvironment();
                        resolveDependenciesTask.resolveDependencies();
                        installAllDependenciesTask.installAllDependencies();
                    }
                });

            project.getGradle().getTaskGraph().whenReady(graph -> {
                    List<Task> tasks = graph.getAllTasks();
                    for (Task task: tasks) {
                        if (task instanceof EtaRepl) {
                            boolean inDaemon = false;
                            Set<Thread> allThreads =
                                Thread.getAllStackTraces().keySet();
                            for (Thread thread: allThreads) {
                                if (thread.getName().contains("Daemon")) {
                                    inDaemon = true;
                                    break;
                                }
                            }
                            if (inDaemon) {
                                throw new GradleException("You are running a REPL task '" + task.getName() + "' in daemon mode, which is not supported.\n\nAdd the flags '--no-daemon -q' for the best REPL experience.");
                            }
                            if (project.getGradle().getStartParameter().getLogLevel().compareTo(LogLevel.QUIET) < 0) {
                                project.getLogger().warn("WARNING: You are running a REPL task without '-q'. You will get a better REPL experience by using that flag.");
                            }
                        }
                    }
                });
        }
    }

    private void configureInjectionTasks() {
        if (isRootProject()) {
            project.getGradle().projectsEvaluated
                (gradle -> gradle.allprojects
                 (p -> {
                     JavaPluginConvention javaConvention =
                         p.getConvention().findPlugin(JavaPluginConvention.class);
                     EtaPlugin etaPlugin = p.getPlugins().findPlugin(EtaPlugin.class);
                     if (javaConvention != null && etaPlugin == null) {
                         javaConvention.getSourceSets().all
                             (sourceSet -> configureSourceSetInjection(p, sourceSet));
                     }
                 }));
        }
    }

    private void configureSourceSetInjection(final Project p, SourceSet sourceSet) {
        EtaInjectDependencies injectDependenciesTask = p.getTasks()
            .create(NamingScheme.getInjectDependenciesTaskName(sourceSet.getName()),
                    EtaInjectDependencies.class);
        injectDependenciesTask.setTargetConfiguration
            (project.provider(() -> sourceSet.getCompileClasspathConfigurationName()));
        injectDependenciesTask.dependsOnProjects();

        sourceSet.setCompileClasspath
            (sourceSet.getCompileClasspath()
             .plus(p.files().builtBy(injectDependenciesTask)));
    }

    private boolean isRootProject() {
        return project == project.getRootProject();
    }

    private void createProguardFiles() {
        ProguardFiles.createAll(project);
    }

    private void configureTestTask() {
        Class<?> testExecuter = null;
        Method method = null;
        try {
            testExecuter = Class.forName("org.gradle.api.internal.tasks.testing.detection.TestExecuter");
            method = Test.class.getDeclaredMethod("setTestExecuter", testExecuter);
        } catch (NoSuchMethodException ne) {
            try {
                testExecuter = Class.forName("org.gradle.api.internal.tasks.testing.TestExecuter");
                method = Test.class.getDeclaredMethod("setTestExecuter", testExecuter);
            } catch (NoSuchMethodException e) {
                project.getLogger().info("Unable to find method with name 'setTestExecuter' in class org.gradle.api.tasks.testing.Test.");
            } catch (ClassNotFoundException e) {
                project.getLogger().info("Unable to find class org.gradle.api.tasks.testing.detection.TestExecuter");
            }
        } catch (ClassNotFoundException cne) {
            project.getLogger().info("Unable to find class org.gradle.api.tasks.testing.detection.TestExecuter");
        }

        if (method == null || testExecuter == null) return;

        method.setAccessible(true);

        final Method setTestExecuterMethod = method;
        final Class<?> testExecuterClass = testExecuter;

        project.getTasks().withType
            (Test.class, test ->
             test.getConvention().getExtraProperties()
             .set("useEtaTest", new SetTestExecuterClosure
                  (test, setTestExecuterMethod, testExecuterClass)));
    }

    private class SetTestExecuterClosure extends Closure {

        private final Test test;
        private final Method method;
        private final Class<?> testExecuter;

        public SetTestExecuterClosure(Test test, Method method, Class<?> testExecuter) {
            super(null);
            this.test = test;
            this.method = method;
            this.testExecuter = testExecuter;
        }

        public Object doCall() {
            Object taskExecuterInstance =
                Proxy.newProxyInstance
                (testExecuter.getClassLoader(), new Class<?>[] { testExecuter },
                 new TestExecuterInvokeHandler());
            try {
                method.invoke(test, taskExecuterInstance);
            } catch (IllegalAccessException e) {
                throw new GradleException("Unable to access 'setTestExecuter'", e);
            } catch (InvocationTargetException e) {
                throw new GradleException("Exception thrown by 'setTestExecuter'", e);
            }
            return null;
        }
    }

    private class TestExecuterInvokeHandler implements InvocationHandler {

        public TestExecuterInvokeHandler() {}

        @Override
        public Object invoke(Object proxy, Method method, Object[] args) {
            if (method.getName().equals("execute")) {
                TestResultProcessor resultProcessor = (TestResultProcessor) args[1];
                Object testSpec = args[0];
                if (testSpec instanceof Test) {
                    execute((Test) testSpec, resultProcessor);
                } else if (testSpec instanceof JvmTestExecutionSpec) {
                    execute((JvmTestExecutionSpec) testSpec, resultProcessor);
                } else {
                    throw new GradleException
                        ("Bad argument to 'execute': " + testSpec.getClass());
                }
            } else if (method.getName().equals("stopNow")) {
                /* TODO: Kill the process if this is called. */
            }
            return null;
        }

        private void execute(JvmTestExecutionSpec spec, TestResultProcessor resultProcessor) {
            execute(execSpec -> {
                    spec.getJavaForkOptions().copyTo(execSpec);
                    execSpec.setClasspath(project.files(spec.getClasspath()));
                }, resultProcessor);
        }

        private void execute(Test test, TestResultProcessor resultProcessor) {
            execute(execSpec -> {
                    test.copyTo(execSpec);
                    execSpec.setClasspath(test.getClasspath());
                }, resultProcessor);
        }

        private void execute(Action<JavaExecSpec> execSpecAction,
                             TestResultProcessor resultProcessor) {
            Object rootTestId = "eta-test-root";
            Object testId = "eta-test";
            resultProcessor.started
                (new DefaultTestSuiteDescriptor(rootTestId, "Eta Root"),
                 new TestStartEvent(System.currentTimeMillis()));
            resultProcessor.started
                (new DefaultTestDescriptor(testId, "eta.main", "Default Eta Test"),
                 new TestStartEvent(System.currentTimeMillis()));
            ExecResult execResult = project.javaexec(execSpec -> {
                    execSpecAction.execute(execSpec);
                    execSpec.setMain("eta.main");
                });
            ResultType resultType =
                (execResult.getExitValue() == 0)? ResultType.SUCCESS : ResultType.FAILURE;
            resultProcessor.completed
                (testId, new TestCompleteEvent(System.currentTimeMillis(), resultType));
            resultProcessor.completed
                (rootTestId, new TestCompleteEvent(System.currentTimeMillis(), resultType));
        }
    }
}
