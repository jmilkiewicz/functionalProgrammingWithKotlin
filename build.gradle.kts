plugins {
    kotlin("jvm") version "1.9.0"
    application
}

group = "me.jakubmilkiewicz"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation("org.hamcrest:hamcrest:2.2")
    testImplementation(kotlin("test"))
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin:2.16.+")
    testImplementation("io.arrow-kt:arrow-core:0.12.1")
    implementation("io.arrow-kt:arrow-core:0.12.1")
    implementation("io.arrow-kt:arrow-fx-coroutines:0.12.1")
    testImplementation("io.arrow-kt:arrow-fx-coroutines:0.12.1")
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(17)
}

application {
    mainClass.set("MainKt")
}