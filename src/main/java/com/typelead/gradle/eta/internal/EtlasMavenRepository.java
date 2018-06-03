package com.typelead.gradle.eta.internal;

import java.io.File;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.FileReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Calendar;
import java.util.TimeZone;
import java.text.SimpleDateFormat;

import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.provider.Provider;

import com.typelead.gradle.utils.ImmutableDAG;
import com.typelead.gradle.utils.PackageInfo;
import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.eta.api.EtaExtension;
import static com.typelead.gradle.utils.PrintHelper.*;

public class EtlasMavenRepository {

    public static final String DEFAULT_GROUP_ID = "eta";
    private final Project project;
    private final File    repositoryDirectory;
    private final Provider<ResolvedExecutable> resolvedEta;
    private String groupId;

    public EtlasMavenRepository(Project project, File repositoryDirectory) {
        this.project = project;
        this.repositoryDirectory = repositoryDirectory;
        this.resolvedEta = project.provider
            (() -> project.getRootProject().getExtensions()
                          .getByType(EtaExtension.class).getEta().get());
    }

    public File getDirectory() {
        return repositoryDirectory;
    }

    public String getGroupId() {
        if (groupId == null) {
            groupId = DEFAULT_GROUP_ID + "-" +
                      resolvedEta.get().getVersion().replace(".", "");
        }
        return groupId;
    }

    public boolean contains(PackageInfo packageInfo) {
        return getPackageVersionDirectory(packageInfo).isDirectory();
    }

    public String getMavenDependency(PackageInfo packageInfo) {
        return getGroupId() + ":" + packageInfo.getName()
                            + ":" + packageInfo.getFullVersion();
    }

    public void installPackages(Collection<PackageInfo> packageInfos,
                                ImmutableDAG<String, PackageInfo> graph) {
        for (PackageInfo packageInfo : packageInfos) {
            if (!contains(packageInfo)) {
                installPackage(packageInfo, graph);
            }
        }
    }

    public void installPackage(PackageInfo packageInfo,
                               ImmutableDAG<String, PackageInfo> graph) {
        writeMavenMetadataFile(packageInfo);
        writePackageContents(packageInfo, graph);
    }

    private void writeMavenMetadataFile(PackageInfo packageInfo) {
        try {
            final File mavenMetadataFile = getMavenMetadataFile(packageInfo);
            final StringBuilder sb = new StringBuilder();
            if (mavenMetadataFile.exists()) {
                BufferedReader in = null;
                try {
                    in = new BufferedReader(new FileReader(mavenMetadataFile));
                    String line = null;
                    while ((line = in.readLine()) != null) {

                        if (line.contains("<lastUpdated>")) {

                            print  (sb, "    <lastUpdated>");
                            print  (sb, getCurrentTime());
                            println(sb, "</lastUpdated>");

                        } else {

                            if (line.contains("</versions>")) {
                                print(sb, "      <version>");
                                print(sb, packageInfo.getFullVersion());
                                println(sb, "</version>");
                            }

                            println(sb, line);
                        }
                    }
                } finally {
                    if (in != null) {
                        in.close();
                    }
                }
            } else {
                println(sb, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                println(sb, "<metadata>");
                print(sb, "  <groupId>");
                print(sb, getGroupId());
                println(sb, "</groupId>");
                print  (sb, "  <artifactId>");
                print  (sb, packageInfo.getName());
                println(sb, "</artifactId>");
                println(sb, "  <versioning>");
                print  (sb, "    <release>");
                print  (sb, packageInfo.getFullVersion());
                println(sb, "</release>");
                println(sb, "    <versions>");
                print  (sb, "      <version>");
                print  (sb, packageInfo.getFullVersion());
                println(sb, "</version>");
                println(sb, "    </versions>");
                print  (sb, "    <lastUpdated>");
                print  (sb, getCurrentTime());
                println(sb, "</lastUpdated>");
                println(sb, "  </versioning>");
                println(sb, "</metadata>");
            }

            mavenMetadataFile.getParentFile().mkdirs();
            Files.write(mavenMetadataFile.toPath(),
                        sb.toString().getBytes(StandardCharsets.UTF_8));
        } catch (IOException ie) {
            throw new GradleException("Failed to write maven-metadata.xml file for "
                                      + packageInfo.getName(), ie);
        }
    }

    private void writePackageContents(PackageInfo packageInfo,
                                      ImmutableDAG<String, PackageInfo> graph) {
        writePomFile(packageInfo, graph);
        copyJarFile(packageInfo);
    }

    private void writePomFile(PackageInfo packageInfo,
                              ImmutableDAG<String, PackageInfo> graph) {
        final StringBuilder sb = new StringBuilder();
        println(sb, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        println(sb, "<project xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd\" xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">");
        println(sb, "  <modelVersion>4.0.0</modelVersion>");
        print  (sb, "  <groupId>");
        print  (sb, getGroupId());
        println(sb, "</groupId>");
        print  (sb, "  <artifactId>");
        print  (sb, packageInfo.getName());
        println(sb, "</artifactId>");
        print  (sb, "  <version>");
        print  (sb, packageInfo.getFullVersion());
        println(sb, "</version>");
        println(sb, "  <dependencies>");
        for (PackageInfo dependency : graph.getNodeValues(packageInfo.getName())) {
            println(sb, "    <dependency>");
            print  (sb, "      <groupId>");
            print  (sb, DEFAULT_GROUP_ID);
            println(sb, "</groupId>");
            print  (sb, "      <artifactId>");
            print  (sb, dependency.getName());
            println(sb, "</artifactId>");
            print  (sb, "      <version>");
            print  (sb, dependency.getFullVersion());
            println(sb, "</version>");
            println(sb, "    </dependency>");
        }

        for (String mavenDependency : packageInfo.getMavenDependencies()) {
            String[] parts = mavenDependency.split(":");
            println(sb, "    <dependency>");
            print  (sb, "      <groupId>");
            print  (sb, parts[0]);
            println(sb, "</groupId>");
            print  (sb, "      <artifactId>");
            print  (sb, parts[1]);
            println(sb, "</artifactId>");
            print  (sb, "      <version>");
            print  (sb, parts[2]);
            println(sb, "</version>");
            println(sb, "    </dependency>");
        }
        println(sb, "  </dependencies>");
        println(sb, "</project>");
        try {
            final File packageVersionDirectory =
                getPackageVersionDirectory(packageInfo);
            packageVersionDirectory.mkdirs();
            Files.write(packageVersionDirectory.toPath()
                        .resolve(getPomFileName(packageInfo)),
                        sb.toString().getBytes(StandardCharsets.UTF_8));
        } catch (IOException ie) {
            throw new GradleException("Failed to write pom file for "
                                      + packageInfo.getIdentifier(), ie);
        }
    }

    private void copyJarFile(PackageInfo packageInfo) {
        final String jarPath = packageInfo.getJarPath();
        try {
            Files.copy(Paths.get(jarPath),
                       getPackageVersionDirectory(packageInfo).toPath()
                       .resolve(getJarFileName(packageInfo)),
                       StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ie) {
            throw new GradleException("Unable to copy " + jarPath +
                                      " into local Eta Maven repository.", ie);
        }
    }

    private static SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");

    static {
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    private static String getCurrentTime() {
        return sdf.format(Calendar.getInstance().getTime());
    }

    private String getJarFileName(PackageInfo packageInfo) {
        return packageInfo.getIdentifier() + ".jar";
    }

    private String getPomFileName(PackageInfo packageInfo) {
        return packageInfo.getIdentifier() + ".pom";
    }

    private File getPackageDirectory(PackageInfo packageInfo) {
        return new File(repositoryDirectory,
                        getGroupIdPath(getGroupId()) + File.separator +
                        packageInfo.getName());
    }

    private String getGroupIdPath(String groupId) {
        return groupId.replace(".", File.separator);
    }

    private File getPackageVersionDirectory(PackageInfo packageInfo) {
        return new File(getPackageDirectory(packageInfo),
                        packageInfo.getFullVersion());
    }

    private File getMavenMetadataFile(PackageInfo packageInfo) {
        return new File(getPackageDirectory(packageInfo), "maven-metadata-local.xml");
    }
}
