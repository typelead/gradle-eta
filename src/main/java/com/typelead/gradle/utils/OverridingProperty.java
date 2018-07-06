package com.typelead.gradle.utils;

import java.util.Optional;
import java.util.function.Predicate;

import org.gradle.api.Transformer;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.Property;

public class OverridingProperty<T> implements Property<T> {
    private Provider<T>  overriding;
    private Predicate<T> considerEphemeral;
    private Property<T>  ephemeral;

    public OverridingProperty(Provider<T> overriding, Optional<T> ephemeralTrigger,
                              Property<T> ephemeral) {
        this(overriding, t -> t == ephemeralTrigger.orElse(null), ephemeral);
    }

    public OverridingProperty(Provider<T> overriding, Predicate<T> considerEphemeral,
                              Property<T> ephemeral) {
        this.overriding = overriding;
        this.considerEphemeral = considerEphemeral;
        this.ephemeral = ephemeral;
    }

    @Override
    public void set(Provider<? extends T> provider) {
        ephemeral.set(provider);
    }

    @Override
    public void set(T value) {
        ephemeral.set(value);
    }

    @Override
    public T get() {
        T val = null;
        if ((val = getOrNull()) == null) {
            throw new IllegalStateException("Provider not supplied with value.");
        } else {
            return val;
        }
    }

    @Override
    public T getOrElse(T def) {
        T val = null;
        if ((val = getOrNull()) == null) {
            return def;
        } else {
            return val;
        }
    }

    @Override
    public T getOrNull() {
        T val = overriding.getOrNull();
        if (val == null || considerEphemeral.test(val)) {
            return ephemeral.getOrNull();
        }
        return val;
    }

    @Override
    public boolean isPresent() {
        return getOrNull() != null;
    }

    @Override
    public <S> Provider<S> map(Transformer<? extends S, ? super T> transformer) {
        throw new UnsupportedOperationException("The map operation is not provided by OverridingProperty!");
    }
}
